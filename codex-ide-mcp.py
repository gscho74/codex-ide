#!/usr/bin/env python3
"""Read-only MCP stdio server for one Emacs session's private editor snapshot.

Codex starts this process through per-invocation -c overrides. It listens on no
network port and never evaluates Elisp, writes project files, or grants approvals.
"""
import json
from pathlib import Path
import sys

MAX_STATE_BYTES = 256 * 1024
TOOL = {
    "name": "get_editor_context",
    "description": (
        "Read the current Emacs file, cursor, selected line range and selected "
        "source, plus diagnostics. Call this whenever the user asks about the "
        "current file, this code, or selected lines; the selection can change "
        "between messages. An empty context means nothing is shared."
    ),
    "inputSchema": {"type": "object", "properties": {}, "additionalProperties": False},
    "annotations": {"readOnlyHint": True, "destructiveHint": False, "openWorldHint": False},
}


def handle(message, state_file):
    """Return one JSON-RPC response, or None for a notification."""
    if not isinstance(message, dict):
        return {"jsonrpc": "2.0", "id": None, "error": {"code": -32600, "message": "Invalid request"}}
    if "id" not in message:
        return None
    request_id = message["id"]
    method = message.get("method")
    params = message.get("params") or {}
    if not isinstance(params, dict):
        return {"jsonrpc": "2.0", "id": request_id,
                "error": {"code": -32602, "message": "Invalid params"}}
    if method == "initialize":
        result = {
            "protocolVersion": params.get("protocolVersion", "2024-11-05"),
            "capabilities": {"tools": {}},
            "serverInfo": {"name": "codex-ide-editor", "version": "0.1.0"},
        }
    elif method == "ping":
        result = {}
    elif method == "tools/list":
        result = {"tools": [TOOL]}
    elif method == "tools/call":
        if params.get("name") != TOOL["name"] or params.get("arguments", {}) != {}:
            return {"jsonrpc": "2.0", "id": request_id,
                    "error": {"code": -32602, "message": "Unknown tool or invalid arguments"}}
        try:
            with state_file.open("rb") as stream:
                raw = stream.read(MAX_STATE_BYTES + 1)
            if len(raw) > MAX_STATE_BYTES:
                raise ValueError("Editor snapshot exceeds size limit")
            state = json.loads(raw)
            if not isinstance(state, dict):
                raise ValueError("Invalid editor snapshot")
            result = {"content": [{"type": "text", "text": json.dumps(state, ensure_ascii=False)}],
                      "isError": False}
        except (OSError, ValueError) as error:
            result = {"content": [{"type": "text", "text": f"Editor context unavailable: {error}"}],
                      "isError": True}
    else:
        return {"jsonrpc": "2.0", "id": request_id,
                "error": {"code": -32601, "message": "Method not supported"}}
    return {"jsonrpc": "2.0", "id": request_id, "result": result}


def main():
    state_file = Path(sys.argv[1])
    for line in sys.stdin:
        try:
            reply = handle(json.loads(line), state_file)
        except ValueError:
            reply = {"jsonrpc": "2.0", "id": None,
                     "error": {"code": -32700, "message": "Parse error"}}
        if reply is not None:
            print(json.dumps(reply, ensure_ascii=False), flush=True)


if __name__ == "__main__":
    main()
