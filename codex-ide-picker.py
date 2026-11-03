#!/usr/bin/env python3
"""Shared project-scoped resume/delete menu for Emacs terminal buffers."""
import argparse
import curses
from datetime import datetime
import json
import os
from pathlib import Path
import selectors
import subprocess
import sys
import time
import unicodedata


class RpcError(RuntimeError):
    pass


class AppServer:
    """Metadata-only stdio client; never starts a model turn."""
    def __init__(self, codex, root):
        self.process = subprocess.Popen(
            [codex, 'app-server', '--listen', 'stdio://'], cwd=root,
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        self.selector = selectors.DefaultSelector()
        self.selector.register(self.process.stdout, selectors.EVENT_READ)
        self.pending = b''
        self.ident = 0

    def send(self, message):
        self.process.stdin.write((json.dumps(message) + '\n').encode())
        self.process.stdin.flush()

    def request(self, method, params):
        self.ident += 1
        ident = self.ident
        self.send({'id': ident, 'method': method, 'params': params})
        deadline = time.monotonic() + 15
        while time.monotonic() < deadline:
            if b'\n' in self.pending:
                line, self.pending = self.pending.split(b'\n', 1)
                message = json.loads(line)
                if message.get('method') and 'id' in message:
                    self.send({'id': message['id'], 'error': {
                        'code': -32601, 'message': 'Picker does not support server requests'}})
                elif message.get('id') == ident:
                    if 'error' in message:
                        raise RpcError(message['error'].get('message', 'App Server request failed'))
                    return message.get('result', {})
            elif self.selector.select(timeout=0.1):
                chunk = os.read(self.process.stdout.fileno(), 65536)
                if not chunk:
                    raise RpcError('App Server exited')
                self.pending += chunk
        raise RpcError(f'{method} timed out')

    def initialize(self):
        self.request('initialize', {'clientInfo': {'name': 'emacs_codex_picker', 'version': '0.5.0'},
                                    'capabilities': {}})
        self.send({'method': 'initialized', 'params': {}})

    def close(self):
        self.selector.close()
        if self.process.stdin:
            try:
                self.process.stdin.close()
            except OSError:
                pass
        try:
            self.process.wait(timeout=1)
        except subprocess.TimeoutExpired:
            self.process.terminate()
            try:
                self.process.wait(timeout=2)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait(timeout=2)
        self.process.stdout.close()


def in_project(thread, root):
    cwd = thread.get('cwd')
    return isinstance(cwd, str) and os.path.isabs(cwd) and Path(cwd).resolve() == root


def project_threads(client, root):
    rows, cursors, ids = [], set(), set()
    cursor = None
    while True:
        params = {'limit': 100, 'cwd': [str(root), str(root) + '/'], 'sortKey': 'updated_at'}
        if cursor:
            params['cursor'] = cursor
        result = client.request('thread/list', params)
        for thread in result.get('data', []):
            ident = thread.get('id')
            if isinstance(ident, str) and ident and ident not in ids and in_project(thread, root):
                rows.append(thread)
                ids.add(ident)
        cursor = result.get('nextCursor')
        if not cursor:
            return rows
        if cursor in cursors:
            raise RpcError('Repeated conversation-list cursor')
        cursors.add(cursor)


def validate_thread(client, ident, root):
    thread = client.request('thread/read', {'threadId': ident, 'includeTurns': False}).get('thread', {})
    if thread.get('id') != ident or not in_project(thread, root):
        raise RpcError('The selected conversation does not belong to this project')
    return thread


def clean(text):
    return ''.join(char for char in str(text) if char.isprintable())


def title(thread):
    return clean(thread.get('name') or thread.get('preview') or '(untitled)')


def cell_width(text):
    return sum(0 if unicodedata.combining(char) else
               2 if unicodedata.east_asian_width(char) in ('W', 'F') else 1 for char in text)


def clipped(text, width):
    output = ''
    for char in clean(text):
        if cell_width(output + char) > width:
            break
        output += char
    return output


def put(screen, row, text, attr=0):
    height, width = screen.getmaxyx()
    if 0 <= row < height and width > 1:
        try:
            screen.addstr(row, 0, clipped(text, width - 1), attr)
        except curses.error:
            pass


def confirm_delete(screen, thread):
    while True:
        screen.erase()
        put(screen, 0, 'Delete conversation', curses.A_BOLD)
        put(screen, 2, title(thread))
        put(screen, 4, 'Permanently delete this conversation?')
        put(screen, 6, 'y: delete   n / Esc: cancel')
        screen.refresh()
        key = screen.get_wch()
        if key in ('y', 'Y'):
            return True
        if key in ('n', 'N', '\x1b', '\n', '\r'):
            return False


def choose(screen, client, root, mode):
    """Run the menu; confirmed deletions refresh it until the user cancels."""
    curses.curs_set(1)
    screen.keypad(True)
    screen.erase()
    put(screen, 0, 'Loading project conversations...')
    screen.refresh()
    client.initialize()
    rows = project_threads(client, root)
    query, selected, notice = '', 0, ''
    deleted_ids = set()
    while True:
        visible = [thread for thread in rows if query.casefold() in title(thread).casefold()]
        selected = min(selected, max(0, len(visible) - 1))
        height, width = screen.getmaxyx()
        page_size = max(1, height - 7)
        start = max(0, selected - page_size + 1)
        screen.erase()
        put(screen, 0, ('Delete' if mode == 'delete' else 'Resume') + ' conversation', curses.A_BOLD)
        put(screen, 1, 'Project: ' + str(root))
        put(screen, 2, 'Search: ' + query)
        put(screen, 3, notice, curses.A_DIM)
        put(screen, 4, 'Updated           Conversation', curses.A_DIM)
        for offset, thread in enumerate(visible[start:start + page_size]):
            stamp = thread.get('updatedAt') or thread.get('createdAt')
            date = datetime.fromtimestamp(stamp).strftime('%Y-%m-%d %H:%M') if isinstance(stamp, (int, float)) else ' ' * 16
            row = ('> ' if start + offset == selected else '  ') + date + '  ' + title(thread)
            put(screen, 5 + offset, row, curses.A_REVERSE if start + offset == selected else 0)
        if not visible:
            put(screen, 5, 'No conversations in this project' if not rows else 'No matching conversations')
        put(screen, height - 1, 'Up/Down: select   Enter: choose   Esc: cancel')
        try:
            screen.move(2, min(max(0, width - 2), cell_width('Search: ' + query)))
        except curses.error:
            pass
        screen.refresh()
        key = screen.get_wch()
        if key == '\x1b':
            return None
        if key in (curses.KEY_DOWN, '\x0e'):
            selected = min(selected + 1, max(0, len(visible) - 1))
        elif key in (curses.KEY_UP, '\x10'):
            selected = max(0, selected - 1)
        elif key in ('\n', '\r', curses.KEY_ENTER) and visible:
            thread = visible[selected]
            if mode != 'delete':
                return thread
            if not confirm_delete(screen, thread):
                continue
            try:
                validate_thread(client, thread['id'], root)
                client.request('thread/delete', {'threadId': thread['id']})
            except (RpcError, OSError, ValueError) as error:
                notice = 'Not deleted: ' + clean(error)
                continue
            deleted_ids.add(thread['id'])
            rows = [row for row in rows if row['id'] not in deleted_ids]
            notice = 'Deleted: ' + title(thread)
            try:
                # Exclude just-deleted IDs even if the server's list index lags.
                rows = [row for row in project_threads(client, root) if row['id'] not in deleted_ids]
            except (RpcError, OSError, ValueError):
                notice += ' (list refresh failed)'
        elif key in (curses.KEY_BACKSPACE, '\x7f', '\b'):
            query, selected = query[:-1], 0
        elif isinstance(key, str) and key.isprintable():
            query, selected = query + key, 0


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--mode', choices=('resume', 'delete'), required=True)
    parser.add_argument('--project-root', required=True)
    parser.add_argument('--codex', required=True)
    parser.add_argument('cli_args', nargs=argparse.REMAINDER)
    options = parser.parse_args()
    root = Path(options.project_root).resolve()
    cli_args = options.cli_args[1:] if options.cli_args[:1] == ['--'] else options.cli_args
    client = AppServer(options.codex, root)
    try:
        picked = curses.wrapper(choose, client, root, options.mode)
        if picked is None:
            return 0
        # Re-read the authoritative cwd after selection/confirmation.
        validate_thread(client, picked['id'], root)
    finally:
        client.close()
    os.chdir(root)
    os.execvpe(options.codex, [options.codex, *cli_args, 'resume', picked['id']], os.environ)


if __name__ == '__main__':
    try:
        sys.exit(main())
    except KeyboardInterrupt:
        sys.exit(0)
    except (RpcError, OSError, ValueError) as error:
        print('Codex conversation menu: ' + clean(error), file=sys.stderr)
        sys.exit(1)
