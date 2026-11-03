;;; codex-ide-terminal.el --- Terminal UI and Emacs context bridge -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: MIT
;;; Commentary:
;; Run the real Codex TUI, with a read-only stdio MCP editor-context server.
;;; Code:
(require 'codex-ide)
(require 'term)
(declare-function vterm "vterm" (&optional arg))
(declare-function vterm-send-string "vterm" (string &optional paste-p))
(declare-function vterm-send-return "vterm" ())
(declare-function ghostel-exec "ghostel" (buffer program &optional args))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel-ime-mode "ghostel-ime" (&optional arg))
(declare-function ghostel--invalidate "ghostel" ())
(defvar vterm-shell)
(defvar vterm-kill-buffer-on-exit)
(defvar ghostel-kill-buffer-on-exit)
(defvar ghostel-glyph-scale-floor)
(defvar ghostel-ime--composition-buffer)
(defvar ghostel-ime--original-input-method-function)
(defvar ghostel--input-mode)
(defvar ghostel--process)
(defvar ghostel--last-send-time)
(defvar hangul-im-keymap)
(defvar hangul-queue)
(defconst codex-ide-terminal--directory
  (file-name-directory (or load-file-name buffer-file-name)))
(defcustom codex-ide-terminal-backend 'auto
  "Emulator for the Codex CLI.  Auto prefers Ghostel, then vterm, then term.
Ghostel and vterm must be installed separately; term is built into Emacs."
  :type '(choice (const auto) (const ghostel) (const vterm) (const term))
  :group 'codex-ide)
(defcustom codex-ide-terminal-inline t
  "Use Codex's inline renderer to retain scrollback and reduce startup redraws."
  :type 'boolean :group 'codex-ide)
(defcustom codex-ide-terminal-ghostel-glyph-scale-floor 0.0
  "Minimum glyph scale in Codex Ghostel buffers.
1.0 keeps Hangul and other fallback glyphs at natural size.  Set to 0.0
to allow Ghostel to shrink glyphs for strict terminal grid alignment."
  :type '(float 0.0 1.0) :group 'codex-ide)
(defvar-local codex-ide-terminal--backend nil)
(defvar-local codex-ide-terminal--runtime nil)
(defvar-local codex-ide-terminal--state-file nil)
(defvar-local codex-ide-terminal--last-state nil)
(defvar-local codex-ide-terminal--timer nil)
(defvar-local codex-ide-terminal--picker-action nil)
(defvar-local codex-ide-terminal--ime-start nil)
(defvar-local codex-ide-terminal--ime-spacing-overlays nil)
(defvar-local codex-ide-terminal--ime-placeholder-overlay nil)

(defun codex-ide-terminal--choose-backend ()
  "Load and return an available terminal emulator."
  (let ((choices (if (eq codex-ide-terminal-backend 'auto)
                     '(ghostel vterm term) (list codex-ide-terminal-backend))))
    (or (seq-find (lambda (backend)
                    (and (memq backend '(ghostel vterm term))
                         (require backend nil t))) choices)
        (user-error "Terminal backend %s is unavailable; install it or choose auto"
                    codex-ide-terminal-backend))))

(defun codex-ide-terminal--arguments (root state-file)
  "Build safe CLI arguments for ROOT and STATE-FILE."
  (let ((python (or (executable-find "python3") (user-error "Python 3 is required")))
        (bridge (expand-file-name "codex-ide-mcp.py" codex-ide-terminal--directory)))
    (unless (file-readable-p bridge) (user-error "Editor bridge is missing: %s" bridge))
    (append
     (list "--sandbox" codex-ide-sandbox "--ask-for-approval" "on-request" "--cd" root)
     (when codex-ide-terminal-inline '("--no-alt-screen"))
     (list
           "-c" (concat "mcp_servers.codex_ide_editor.command=" (json-serialize python))
           "-c" (concat "mcp_servers.codex_ide_editor.args="
                         (json-serialize (vector bridge state-file)))
           "-c" "mcp_servers.codex_ide_editor.enabled=true"))))

(defun codex-ide-terminal--command (codex root state-file action)
  "Build the CLI command, or the shared resume/delete picker for ACTION."
  (let ((args (codex-ide-terminal--arguments root state-file)))
    (if (not action)
        (cons codex args)
      (let ((picker (expand-file-name "codex-ide-picker.py" codex-ide-terminal--directory))
            (python (or (executable-find "python3") (user-error "Python 3 is required"))))
        (unless (file-readable-p picker) (user-error "Conversation picker missing: %s" picker))
        (append (list python picker "--mode" (if (eq action 'delete) "delete" "resume")
                      "--project-root" root "--codex" codex "--") args)))))

(defun codex-ide-terminal--publish-context ()
  "Atomically publish this terminal's current editor state to its MCP server."
  (when codex-ide-terminal--timer
    (cancel-timer codex-ide-terminal--timer)
    (setq codex-ide-terminal--timer nil))
  (when (and codex-ide-terminal--runtime
             (file-directory-p codex-ide-terminal--runtime))
    (let* ((state (json-serialize
                   (codex-ide--obj "projectRoot" codex-ide--cwd
                                   "label" (codex-ide--context-label)
                                   "context" (codex-ide--context))))
           (temporary nil))
      (unless (equal state codex-ide-terminal--last-state)
        (unwind-protect
            (progn
              (setq temporary (make-temp-file
                               (expand-file-name "state-" codex-ide-terminal--runtime)))
              (let ((coding-system-for-write 'utf-8-unix))
                (write-region state nil temporary nil 'silent))
              (rename-file temporary codex-ide-terminal--state-file t)
              (setq codex-ide-terminal--last-state state))
          (when (and temporary (file-exists-p temporary)) (delete-file temporary)))))))

(defun codex-ide-terminal--schedule-context ()
  "Coalesce editor events before updating the terminal's MCP state."
  (when (bound-and-true-p codex-ide-terminal-mode)
    (when codex-ide-terminal--timer (cancel-timer codex-ide-terminal--timer))
    (let ((buffer (current-buffer)))
      (setq codex-ide-terminal--timer
            (run-at-time 0.1 nil
                         (lambda ()
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (codex-ide-terminal--publish-context)))))))))

(defun codex-ide-terminal--focus ()
  "Select this session as the target for editor commands."
  (setq codex-ide--active-id codex-ide--thread-id)
  (when codex-ide-terminal--timer (codex-ide-terminal--publish-context)))

(defun codex-ide-terminal--cleanup ()
  "Remove this terminal's private editor-state files and registry entry."
  (when codex-ide-terminal--timer (cancel-timer codex-ide-terminal--timer))
  (setq codex-ide-terminal--timer nil)
  (when (eq (gethash codex-ide--thread-id codex-ide--sessions) (current-buffer))
    (remhash codex-ide--thread-id codex-ide--sessions))
  (when (and codex-ide-terminal--runtime (file-directory-p codex-ide-terminal--runtime))
    (delete-directory codex-ide-terminal--runtime t))
  (setq codex-ide-terminal--runtime nil))

(defvar-local codex-ide-terminal--right-alt-guard nil)
(defvar-local codex-ide-terminal--right-alt-deadline 0)

(defun codex-ide-terminal--finish-right-alt-guard ()
  "End the brief right-Alt modifier guard on ordinary input or timeout."
  (when (and codex-ide-terminal--right-alt-guard
             (or (> (float-time) codex-ide-terminal--right-alt-deadline)
                 (not (eq this-command #'codex-ide-terminal--right-alt-printable))))
    (setq codex-ide-terminal--right-alt-guard nil)))

(defun codex-ide-terminal--guard-right-alt ()
  "Guard Codex input against Alt still held just after a right-Alt toggle.
The guard lasts at most 180 ms and ends on the first ordinary command.
It never changes modifier handling in other buffers."
  (when (and (bound-and-true-p codex-ide-terminal-mode)
             (eq codex-ide-terminal--backend 'ghostel))
    (setq codex-ide-terminal--right-alt-guard t
          codex-ide-terminal--right-alt-deadline (+ (float-time) 0.18))
    (add-hook 'pre-command-hook #'codex-ide-terminal--finish-right-alt-guard nil t)))

(defun codex-ide-terminal--right-alt-printable ()
  "Recover printable input carrying the right-Alt toggle's residual modifier."
  (interactive)
  (if (and codex-ide-terminal--right-alt-guard
           (<= (float-time) codex-ide-terminal--right-alt-deadline))
      (let* ((basic (event-basic-type last-command-event))
             (key (if (and (memq 'shift (event-modifiers last-command-event))
                           (characterp basic)) (upcase basic) basic))
             (events (if (and current-input-method input-method-function)
                         (funcall input-method-function key)
                       (list key))))
        ;; Hangul inserts and commits through ghostel-ime itself.  Only
        ;; pass-through characters (English, digits, punctuation) remain.
        (dolist (event events)
          (if (characterp event)
              (progn
                (ghostel--on-user-input)
                (ghostel--send-string (encode-coding-string (string event) 'utf-8)))
            (setq unread-command-events
                  (append unread-command-events (list event))))))
    ;; If a timeout expired after key lookup, dispatch its normal binding.
    (let ((codex-ide-terminal--right-alt-guard nil))
      (let ((command (key-binding (this-command-keys-vector) t)))
        (when (and (commandp command)
                   (not (eq command #'codex-ide-terminal--right-alt-printable)))
          (call-interactively command))))))

(defvar codex-ide-terminal--right-alt-map
  (let ((map (make-sparse-keymap)))
    (dotimes (offset 95)
      (let ((key (+ 32 offset)))
        (dolist (modifiers '((meta) (control meta)))
          (define-key map (vector (event-convert-list (append modifiers (list key))))
                      #'codex-ide-terminal--right-alt-printable))))
    map))

(defvar codex-ide-terminal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'codex-ide-menu)
    (define-key map (kbd "C-c C-p") #'codex-ide-send-prompt)
    (define-key map (kbd "C-c C-x") #'codex-ide-clear-selection)
    (define-key map (kbd "C-c C-k") #'codex-ide-interrupt)
    map))
(defvar codex-ide-terminal--emulation-map-alist
  `((codex-ide-terminal-mode . ,codex-ide-terminal-mode-map)))

(setq codex-ide-terminal--emulation-map-alist
      (assq-delete-all 'codex-ide-terminal--right-alt-guard
                      codex-ide-terminal--emulation-map-alist))
(add-to-list 'codex-ide-terminal--emulation-map-alist
             (cons 'codex-ide-terminal--right-alt-guard
                   codex-ide-terminal--right-alt-map))

(defun codex-ide-terminal--enable-ghostel-ime ()
  "Forward committed Hangul syllables through Ghostel's Lisp IME integration.
Keep composition edits in Emacs until commit instead of sending each jamo.
Preserve the user's input method and enable integration only in this buffer."
  (when (and (eq codex-ide-terminal--backend 'ghostel)
             (derived-mode-p 'ghostel-mode)
             (require 'ghostel-ime nil t))
    (unless (advice-member-p #'codex-ide-terminal--around-ghostel-ime
                             'ghostel-ime--wrap-input-method)
      (advice-add 'ghostel-ime--wrap-input-method :around
                  #'codex-ide-terminal--around-ghostel-ime))
    (add-hook 'after-change-functions
              #'codex-ide-terminal--update-ime-placeholder 95 t)
    (with-eval-after-load 'hangul
      (advice-remove 'hangul-delete-backward-char
                     #'codex-ide-terminal--delete-hangul-syllable)
      (unless (advice-member-p #'codex-ide-terminal--atomic-hangul-update
                               'hangul-insert-character)
        (advice-add 'hangul-insert-character :around
                    #'codex-ide-terminal--atomic-hangul-update))
      (unless (advice-member-p #'codex-ide-terminal--stream-confirmed-ime
                               'hangul-delete-backward-char)
        (advice-add 'hangul-delete-backward-char :after
                    #'codex-ide-terminal--stream-confirmed-ime)))
    (ghostel-ime-mode 1)))

;; Reloading must also remove the old override that erased entire preedit
;; syllables.  The standard Hangul queue decides Jamo vs. committed deletion.
(advice-remove 'hangul-delete-backward-char
               #'codex-ide-terminal--delete-hangul-syllable)

(defun codex-ide-terminal--atomic-hangul-update (function &rest args)
  "Show only the completed replacement of a Hangul preedit character."
  (if (and (bound-and-true-p codex-ide-terminal-mode)
           (eq codex-ide-terminal--backend 'ghostel))
      (let ((inhibit-redisplay t))
        (prog1 (apply function args)
          (codex-ide-terminal--stream-confirmed-ime)))
    (apply function args)))

(defun codex-ide-terminal--clear-ime-spacing ()
  "Remove preedit padding without touching the composed characters."
  (mapc #'delete-overlay codex-ide-terminal--ime-spacing-overlays)
  (setq codex-ide-terminal--ime-spacing-overlays nil))

(defvar-local codex-ide-terminal--ime-padding-cache nil)

(defun codex-ide-terminal--space-ime (&optional changed-start changed-end)
  "Update terminal-cell padding only around changed preedit characters."
  (when (and (display-graphic-p)
             (eq (bound-and-true-p ghostel-ime--composition-buffer) (current-buffer))
             (markerp codex-ide-terminal--ime-start)
             (marker-position codex-ide-terminal--ime-start))
    (unless (hash-table-p codex-ide-terminal--ime-padding-cache)
      (setq codex-ide-terminal--ime-padding-cache (make-hash-table :test 'equal)))
    (let* ((end (min (point) (or changed-end (point))))
           (start (max (marker-position codex-ide-terminal--ime-start)
                       (or changed-start (1- end))))
           (cell (* 2 (default-font-width)))
           (unused (cl-remove-if-not
                    (lambda (o) (overlay-get o 'codex-ime-padding))
                    (delete-dups
                     (append (overlays-in start end)
                             ;; A zero-length overlay exactly at END is
                             ;; excluded by a nonempty overlays-in range.
                             (cl-remove-if-not
                              (lambda (o) (= (overlay-start o) (overlay-end o)))
                              (overlays-in end end)))))))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (when (eq (aref char-script-table (char-after)) 'hangul)
            (let* ((char (char-after))
                   (font (font-at 0 nil (string char)))
                   (key (list font char cell))
                   (padding (gethash key codex-ide-terminal--ime-padding-cache)))
              (when (and font (null padding))
                (let* ((glyphs (font-get-glyphs font 0 1 (string char)))
                       (glyph (and glyphs (aref glyphs 0))))
                  (when glyph
                    (setq padding (max 0 (- cell (aref glyph 4))))
                    (puthash key padding codex-ide-terminal--ime-padding-cache))))
              (when (and padding (> padding 0))
                (let* ((overlay (or (pop unused)
                                    (let ((o (make-overlay (point) (1+ (point)))))
                                      (overlay-put o 'codex-ime-padding t)
                                      (push o codex-ide-terminal--ime-spacing-overlays)
                                      o)))
                       (display `(space :width (,padding)))
                       (face (or (get-text-property (point) 'face)
                                 (and (overlayp (bound-and-true-p codex-ide-terminal--ime-row-overlay))
                                      (overlay-get codex-ide-terminal--ime-row-overlay 'face))))
                       (old (overlay-get overlay 'after-string)))
                  (unless (and (= (overlay-start overlay) (point))
                               (= (overlay-end overlay) (1+ (point))))
                    (move-overlay overlay (point) (1+ (point))))
                  (unless (and old (equal (get-text-property 0 'display old) display)
                               (equal (get-text-property 0 'face old) face))
                    (overlay-put overlay 'after-string
                                 (propertize " " 'display display 'face face)))))))
          (forward-char 1)))
      ;; Deletion can leave a zero-length overlay at END.  It is included
      ;; by overlays-in even when START=END, so stale cursor padding clears.
      (dolist (overlay unused)
        (delete-overlay overlay)
        (setq codex-ide-terminal--ime-spacing-overlays
              (delq overlay codex-ide-terminal--ime-spacing-overlays))))))

(defun codex-ide-terminal--clear-ime-placeholder ()
  "Restore Codex's placeholder without modifying terminal contents."
  (when (overlayp codex-ide-terminal--ime-placeholder-overlay)
    (delete-overlay codex-ide-terminal--ime-placeholder-overlay))
  (setq codex-ide-terminal--ime-placeholder-overlay nil))

(defun codex-ide-terminal--update-ime-placeholder (&optional start end _old-length)
  "Hide the empty Codex prompt hint while local Lisp IME preedit is visible."
  ;; Hangul replaces a syllable by deleting then inserting.  Keep the old
  ;; mask and padding during replacement; insertion or completed Backspace
  ;; refreshes them once.
  (unless (and start end (= start end))
    (codex-ide-terminal--space-ime start end)
    (codex-ide-terminal--refresh-ime-placeholder)))

(defun codex-ide-terminal--refresh-ime-placeholder ()
  "Refresh the hint mask after a completed preedit insertion."
  (when (and (bound-and-true-p codex-ide-terminal-mode)
             (eq codex-ide-terminal--backend 'ghostel)
             (eq (bound-and-true-p ghostel-ime--composition-buffer) (current-buffer))
             (markerp codex-ide-terminal--ime-start))
    (let ((case-fold-search t))
      (if (and (> (point) codex-ide-terminal--ime-start)
               (looking-at "[ \t]*Ask Codex to do anything"))
          (let ((start (point)) (end (line-end-position)))
            (unless (overlayp codex-ide-terminal--ime-placeholder-overlay)
              (setq codex-ide-terminal--ime-placeholder-overlay
                    (make-overlay start end nil t nil)))
            (move-overlay codex-ide-terminal--ime-placeholder-overlay start end)
            ;; Normally retain native faces and width.  During local soft
            ;; wrapping, the row overlay paints the background so the hint
            ;; can collapse without adding artificial continuation rows.
            (let ((mask (buffer-substring start end)))
              (dotimes (index (length mask)) (aset mask index ?\s))
              (remove-text-properties 0 (length mask) '(display nil composition nil) mask)
              (overlay-put codex-ide-terminal--ime-placeholder-overlay 'display
                           (if (overlayp codex-ide-terminal--ime-row-overlay) "" mask))))
        (codex-ide-terminal--clear-ime-placeholder)))))

(defvar codex-ide-terminal--streaming-ime nil)
(defvar codex-ide-terminal--streamed-ime-text nil)
(defvar codex-ide-terminal--stream-start-position nil)

(defun codex-ide-terminal--send-ime-delta (text)
  "Make the already sent IME prefix match TEXT without duplicate input."
  (when (ghostel--terminal-input-mode-p)
    (let ((common 0)
          (old codex-ide-terminal--streamed-ime-text))
      (while (and (< common (length old)) (< common (length text))
                  (= (aref old common) (aref text common)))
        (setq common (1+ common)))
      ;; Backspace uses Ghostel's key encoder, including keyboard protocols.
      (dotimes (_ (- (length old) common))
        (ghostel--send-encoded "backspace" "")
        (setq codex-ide-terminal--streamed-ime-text
              (substring codex-ide-terminal--streamed-ime-text 0 -1)))
      (when (< common (length text))
        (ghostel--send-string (encode-coding-string (substring text common) 'utf-8))
        (setq codex-ide-terminal--streamed-ime-text text)))))

(defun codex-ide-terminal--stream-confirmed-ime (&rest _args)
  "Send completed syllables while retaining the last syllable as preedit."
  (when (and codex-ide-terminal--streaming-ime
             (eq (bound-and-true-p ghostel-ime--composition-buffer) (current-buffer))
             (markerp codex-ide-terminal--ime-start)
             (marker-position codex-ide-terminal--ime-start))
    (let* ((start (marker-position codex-ide-terminal--ime-start))
           (overlay (bound-and-true-p quail-overlay))
           (end (if (and (overlayp overlay)
                         (eq (overlay-buffer overlay) (current-buffer))
                         (overlay-start overlay)
                         (<= start (overlay-start overlay))
                         (<= (overlay-start overlay) (point)))
                    (overlay-start overlay)
                  (point))))
      (when (< start codex-ide-terminal--stream-start-position)
        ;; Backspacing past all local preedit also removed preceding input.
        (codex-ide-terminal--send-ime-delta "")
        (dotimes (_ (- codex-ide-terminal--stream-start-position start))
          (ghostel--send-encoded "backspace" "")))
      (setq codex-ide-terminal--stream-start-position start)
      (when (<= start end)
        (codex-ide-terminal--send-ime-delta
         (buffer-substring-no-properties start end))))))

(defun codex-ide-terminal--ime-space ()
  "Display a typed space immediately, and stream its committed prefix once.
Keep composing locally after the space so the next syllable cannot jump
back to a terminal cursor whose asynchronous echo has not caught up."
  (interactive)
  (let ((inhibit-read-only t) (inhibit-redisplay t))
    (setq hangul-queue (make-vector 6 0))
    ;; Plain insert drops terminal faces, leaving a default-background gap
    ;; and causing subsequent self-inserted Hangul to inherit that gap.
    (insert-and-inherit " ")
    (when (overlayp (bound-and-true-p quail-overlay))
      (move-overlay quail-overlay (point) (point)))
    (codex-ide-terminal--stream-confirmed-ime)
    (codex-ide-terminal--space-ime)
    (codex-ide-terminal--refresh-ime-placeholder)))

(defun codex-ide-terminal--ime-backspace ()
  "Delete Jamo only inside local preedit; leave rendered UI to the terminal."
  (interactive)
  (if (and (markerp codex-ide-terminal--ime-start)
           (marker-position codex-ide-terminal--ime-start)
           (<= (point) codex-ide-terminal--ime-start))
      (progn
        ;; No local preedit remains.  Exit Hangul's read loop and replay one
        ;; ordinary Backspace.  The terminal owns preceding committed input
        ;; and clamps deletion at its prompt; Emacs must not delete that UI.
        (codex-ide-terminal--clear-ime-spacing)
        (codex-ide-terminal--clear-ime-placeholder)
        (setq unread-command-events (cons 127 unread-command-events))
        (throw 'exit-input-loop nil))
    (let ((inhibit-redisplay t))
      (prog1 (hangul-delete-backward-char)
        ;; after-change skips deletion to avoid showing intermediate Hangul
        ;; replacements.  Refresh the hint after this completed deletion;
        ;; remove collapsed padding before the visible cursor is redisplayed.
        (codex-ide-terminal--space-ime)
        (codex-ide-terminal--refresh-ime-placeholder)))))

(defun codex-ide-terminal--commit-ime (key)
  "Translate KEY and coalesce local preedit removal with terminal echo.
Only the commit handoff inhibits redisplay; composing and reading keys do
not.  The output wait is bounded to one 60-Hz frame and never forces an
unfinished synchronized terminal frame to render."
  (let* ((buffer (current-buffer))
         (ghostel-ime--composition-buffer buffer)
         (original ghostel-ime--original-input-method-function)
         ;; GTK can deliver DEL or the backspace symbol.  Both must check
         ;; the local preedit boundary before deleting renderer-owned text.
         (hangul-im-keymap
          (when (boundp 'hangul-im-keymap)
            (let ((map (copy-keymap hangul-im-keymap)))
              (define-key map [backspace] #'codex-ide-terminal--ime-backspace)
              (define-key map (vector 127) #'codex-ide-terminal--ime-backspace)
              (define-key map " " #'codex-ide-terminal--ime-space)
              map)))
         (before (copy-marker (point)))
         (codex-ide-terminal--streaming-ime
          (memq original '(hangul2-input-method hangul3-input-method hangul390-input-method)))
         (codex-ide-terminal--streamed-ime-text "")
         (codex-ide-terminal--stream-start-position (point)))
    (if (or (null original) (eq original #'list))
        (list key)
      (let ((events (let ((inhibit-read-only t) (buffer-read-only nil))
                      (funcall original key))))
        (when (> (point) before)
          (let ((text (buffer-substring-no-properties before (point)))
                (inhibit-redisplay t))
            ;; Keep forwarding inhibited during local deletion, just as in
            ;; ghostel-ime--wrap-input-method.
            (let ((inhibit-read-only t)) (delete-region before (point)))
            (codex-ide-terminal--clear-ime-placeholder)
            (codex-ide-terminal--clear-ime-spacing)
            (when (ghostel--terminal-input-mode-p)
              (if codex-ide-terminal--streaming-ime
                  (codex-ide-terminal--send-ime-delta text)
                (ghostel--send-string (encode-coding-string text 'utf-8)))
              ;; The terminal renderer must be allowed to consume the echo
              ;; now that composition has finished, before redisplay resumes.
              (let ((ghostel-ime--composition-buffer nil))
                (when (process-live-p ghostel--process)
                  (accept-process-output ghostel--process 0.016))
                (when (bound-and-true-p ghostel--pending-redraw)
                  (ghostel--redraw-now buffer))))))
        (set-marker before nil)
        events))))

(defvar-local codex-ide-terminal--last-echo-render-time nil)

(defun codex-ide-terminal--record-echo-render (buffer &rest _args)
  "Remember successful terminal frames for preedit handoff ordering."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (bound-and-true-p codex-ide-terminal-mode)
                 (not (bound-and-true-p ghostel--pending-redraw)))
        (setq codex-ide-terminal--last-echo-render-time (current-time))))))

(defun codex-ide-terminal--flush-before-ime (key)
  "Consume pending input echo before Hangul freezes native redraws.
For example, the preceding space must reach the display before the next
word's preedit starts.  Wait at most 32 ms, only for recently sent input
not followed by a successful frame.  Never consume keyboard events."
  (when (and (characterp key)
             (or (<= ?a key ?z) (<= ?A key ?Z))
             (bound-and-true-p ghostel--last-send-time)
             (or (null codex-ide-terminal--last-echo-render-time)
                 (time-less-p codex-ide-terminal--last-echo-render-time
                              ghostel--last-send-time))
             (< (float-time (time-subtract (current-time) ghostel--last-send-time)) 0.25)
             (process-live-p ghostel--process)
             (not (eq (bound-and-true-p ghostel-ime--composition-buffer) (current-buffer))))
    (let ((sent ghostel--last-send-time)
          (deadline (+ (float-time) 0.032)))
      (while (and (< (float-time) deadline)
                  (process-live-p ghostel--process)
                  (or (null codex-ide-terminal--last-echo-render-time)
                      (time-less-p codex-ide-terminal--last-echo-render-time sent)))
        (accept-process-output ghostel--process
                               (max 0 (min 0.016 (- deadline (float-time)))))
        (when (bound-and-true-p ghostel--pending-redraw)
          ;; Respect native synchronized-output boundaries; do not force.
          (ghostel--redraw-now (current-buffer)))))))

(defun codex-ide-terminal--restore-ime-cursor (&optional key)
  "Start local IME composition at the terminal's actual editing position.
Do not use point-max: the live cursor can be inside existing prompt text.
Leave Ghostel line mode alone because it owns its local editing position."
  (unless (eq ghostel--input-mode 'line)
    (ghostel--on-user-input)
    (codex-ide-terminal--flush-before-ime key)
    (when (ghostel--terminal-input-mode-p)
      (let ((cursor (ghostel-cursor-point)))
        (when (and (integer-or-marker-p cursor)
                   (<= (point-min) cursor) (<= cursor (point-max)))
          (goto-char cursor)
          (when (eq (window-buffer (selected-window)) (current-buffer))
            (set-window-point (selected-window) (point))))))))

(defvar-local codex-ide-terminal--ime-row-overlay nil)
(defvar-local codex-ide-terminal--ime-tail-overlay nil)

(defun codex-ide-terminal--end-ime-wrap ()
  "Remove temporary preedit row painting and terminal-padding masking."
  (dolist (overlay (list codex-ide-terminal--ime-row-overlay
                         codex-ide-terminal--ime-tail-overlay))
    (when (overlayp overlay) (delete-overlay overlay)))
  (setq codex-ide-terminal--ime-row-overlay nil
        codex-ide-terminal--ime-tail-overlay nil))

(defun codex-ide-terminal--begin-ime-wrap ()
  "Allow local preedit to wrap without wrapping native trailing padding."
  (codex-ide-terminal--end-ime-wrap)
  (let* ((begin (line-beginning-position))
         (end (line-end-position))
         (background (or (codex-ide-terminal--background-at (point))
                         (codex-ide-terminal--background-at begin))))
    ;; Paint all soft-wrapped screen rows using the input background, even
    ;; where native terminal padding is hidden.  No real newline is added.
    (setq codex-ide-terminal--ime-row-overlay
          (make-overlay begin (min (point-max) (1+ end)) nil nil t))
    (when (stringp background)
      (let ((face `(:background ,background :extend t)))
        (overlay-put codex-ide-terminal--ime-row-overlay 'face face)
        ;; Display strings do not reliably inherit the surrounding overlay
        ;; face.  Paint continuation indentation explicitly as well.
        (setq wrap-prefix (propertize "  " 'face face))))
    (save-excursion
      (goto-char end)
      (skip-chars-backward " \t " (marker-position codex-ide-terminal--ime-start))
      (when (< (point) end)
        ;; Front advancement leaves newly composed characters outside the
        ;; hidden native padding, including after deleting all preedit.
        (setq codex-ide-terminal--ime-tail-overlay
              (make-overlay (point) end nil t nil))
        (overlay-put codex-ide-terminal--ime-tail-overlay 'display "")))))

(defun codex-ide-terminal--around-ghostel-ime (function &rest args)
  "Mask local preedit and smooth its handoff to terminal echo."
  (if (not (and (bound-and-true-p codex-ide-terminal-mode)
                (eq codex-ide-terminal--backend 'ghostel)))
      (apply function args)
    (let ((buffer (current-buffer)))
      (when (and ghostel-ime--original-input-method-function
                 (not (eq ghostel-ime--original-input-method-function #'list)))
        (codex-ide-terminal--restore-ime-cursor (car args)))
      (setq codex-ide-terminal--ime-start (copy-marker (point)))
      (unwind-protect
          (if (eq ghostel--input-mode 'line)
              (apply function args)
            (let ((truncate-lines nil)
                  (truncate-partial-width-windows nil)
                  (word-wrap nil)
                  (wrap-prefix "  "))
              (codex-ide-terminal--begin-ime-wrap)
              (apply #'codex-ide-terminal--commit-ime args)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (codex-ide-terminal--end-ime-wrap)
            (codex-ide-terminal--clear-ime-placeholder)
            (codex-ide-terminal--clear-ime-spacing)
            (set-marker codex-ide-terminal--ime-start nil)
            (setq codex-ide-terminal--ime-start nil)))))))

(defvar-local codex-ide-terminal--row-overlay nil)
(defvar-local codex-ide-terminal--row-height nil)

(defun codex-ide-terminal--stabilize-row-height (&rest _change)
  "Keep terminal rows at the height also used by Ghostel's grid sizing."
  (when codex-ide-terminal--row-height
    (unless (overlayp codex-ide-terminal--row-overlay)
      (setq codex-ide-terminal--row-overlay
            (make-overlay (point-min) (point-max) nil nil t)))
    (unless (and (= (overlay-start codex-ide-terminal--row-overlay) (point-min))
                 (= (overlay-end codex-ide-terminal--row-overlay) (point-max)))
      (move-overlay codex-ide-terminal--row-overlay (point-min) (point-max)))
    (unless (equal (overlay-get codex-ide-terminal--row-overlay 'line-height)
                   codex-ide-terminal--row-height)
      (overlay-put codex-ide-terminal--row-overlay 'line-height
                   codex-ide-terminal--row-height))
    ;; The fixed line-height already includes the reserved vertical space.
    (unless (eq (overlay-get codex-ide-terminal--row-overlay 'line-spacing) 0)
      (overlay-put codex-ide-terminal--row-overlay 'line-spacing 0))))

(defun codex-ide-terminal--configure-row-height ()
  "Keep the original Latin row height in redisplay and grid sizing."
  (when (display-graphic-p)
    (setq-local codex-ide-terminal--row-height (default-font-height))
    (setq-local line-spacing 0)
    (add-hook 'after-change-functions
              #'codex-ide-terminal--stabilize-row-height 99 t)
    (codex-ide-terminal--stabilize-row-height)))

(defvar-local codex-ide-terminal--padding-overlays nil)

(defun codex-ide-terminal--compact-input-background (buffer &rest _args)
  "Leave the composer background on input rows, not blank padding rows.
Preserve terminal characters and row geometry so scrolling is unaffected."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (bound-and-true-p codex-ide-terminal-mode)
                 (eq codex-ide-terminal--backend 'ghostel)
                 (not (bound-and-true-p ghostel--pending-redraw))
                 (not (eq (bound-and-true-p ghostel-ime--composition-buffer)
                          (current-buffer))))
        (let ((unused codex-ide-terminal--padding-overlays)
              active positions background)
          (save-excursion
            (goto-char (point-max))
            (when (re-search-backward "^› " (max (point-min) (- (point-max) 12000)) t)
              (let* ((input (line-beginning-position))
                     (face (get-text-property input 'face)))
                (setq background (and (listp face) (plist-get face :background)))
                (when background
                  (forward-line -1)
                  (when (and (< (point) input)
                             (looking-at "[ \t ]*$")
                             (equal background
                                    (plist-get (get-text-property (point) 'face) :background)))
                    (push (cons (point) (line-end-position)) positions))
                  (goto-char input)
                  (forward-line 1)
                  ;; Multiline input keeps its background on every text row.
                  (let ((rows 0))
                    (while (and (< (point) (point-max)) (< rows 32)
                                (equal background
                                       (plist-get (get-text-property (point) 'face) :background))
                                (not (looking-at "[ \t ]*$")))
                      (setq rows (1+ rows))
                      (forward-line 1)))
                  (when (and (< (point) (point-max))
                             (looking-at "[ \t ]*$")
                             (equal background
                                    (plist-get (get-text-property (point) 'face) :background)))
                    (push (cons (point) (line-end-position)) positions))))))
          (dolist (range positions)
            (when (< (car range) (cdr range))
              (let ((overlay (or (pop unused) (make-overlay (car range) (cdr range)))))
                (move-overlay overlay (car range) (cdr range))
                (overlay-put overlay 'priority 100)
                (overlay-put overlay 'face
                             `(:background ,(face-background 'default nil t) :extend t))
                (push overlay active))))
          (mapc #'delete-overlay unused)
          (setq codex-ide-terminal--padding-overlays active))))))

(defface codex-ide-terminal-user-message
  '((t (:background "#50535b")))
  "Brighter gray background for submitted user messages."
  :group 'codex-ide)
(defvar-local codex-ide-terminal--user-message-overlays nil)

(defun codex-ide-terminal--background-at (position)
  "Return the explicit terminal background at POSITION, if present."
  (let ((face (get-text-property position 'face)))
    (and (listp face) (plist-get face :background))))

(defun codex-ide-terminal--brighten-user-messages (buffer &rest _args)
  "Brighten submitted prompt blocks without changing active input or text."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (bound-and-true-p codex-ide-terminal-mode)
                 (eq codex-ide-terminal--backend 'ghostel)
                 (not (bound-and-true-p ghostel--pending-redraw))
                 (not (eq (bound-and-true-p ghostel-ime--composition-buffer)
                          (current-buffer))))
        (let ((unused codex-ide-terminal--user-message-overlays)
              (cursor (and (fboundp 'ghostel-cursor-point) (ghostel-cursor-point)))
              active last-prompt)
          (save-excursion
            (goto-char (point-max))
            (when (re-search-backward "^› " nil t)
              (setq last-prompt (point)))
            (goto-char (point-min))
            (while (re-search-forward "^› " nil t)
              (let* ((prompt (line-beginning-position))
                     (start prompt)
                     (background (codex-ide-terminal--background-at prompt))
                     end)
                ;; Avoid approval choices and other prompts with colored faces.
                (when (member background '("#333540" "#2e303a"))
                  (save-excursion
                    (goto-char prompt)
                    (forward-line -1)
                    (when (and (< (point) prompt) (looking-at "[ \t ]*$")
                               (equal background (codex-ide-terminal--background-at (point))))
                      (setq start (point)))
                    (goto-char prompt)
                    (forward-line 1)
                    (while (and (< (point) (point-max))
                                (equal background (codex-ide-terminal--background-at (point))))
                      (forward-line 1))
                    (setq end (point)))
                  (unless (or (and (integer-or-marker-p cursor)
                                   (<= start cursor) (< cursor end))
                              (and (null cursor) (eq prompt last-prompt)))
                    (let ((overlay (or (pop unused) (make-overlay start end nil t nil))))
                      (move-overlay overlay start end)
                      (overlay-put overlay 'priority 50)
                      (overlay-put overlay 'face 'codex-ide-terminal-user-message)
                      (push overlay active)))))))
          (mapc #'delete-overlay unused)
          (setq codex-ide-terminal--user-message-overlays (nreverse active)))))))

;; Undo the previous cursor customization when this file is reloaded.
(advice-remove 'ghostel--apply-cursor-style #'codex-ide-terminal--thin-cursor)

(defun codex-ide-terminal--setup-display ()
  "Show input state and fit Hangul within the original terminal rows."
  (setq-local mode-line-format
              '(" " mode-line-mule-info " " (:eval (codex-ide--context-label))))
  (when (and (eq codex-ide-terminal--backend 'ghostel)
             (derived-mode-p 'ghostel-mode))
    (unless (advice-member-p #'codex-ide-terminal--compact-input-background
                             'ghostel--redraw-now)
      (advice-add 'ghostel--redraw-now :after
                  #'codex-ide-terminal--compact-input-background))
    (unless (advice-member-p #'codex-ide-terminal--brighten-user-messages
                             'ghostel--redraw-now)
      (advice-add 'ghostel--redraw-now :after
                  #'codex-ide-terminal--brighten-user-messages))
    (codex-ide-terminal--brighten-user-messages (current-buffer))
    (unless (advice-member-p #'codex-ide-terminal--record-echo-render
                             'ghostel--redraw-now)
      (advice-add 'ghostel--redraw-now :after
                  #'codex-ide-terminal--record-echo-render))
    (when (fboundp 'ghostel--apply-cursor-style)
      (ghostel--apply-cursor-style))
    (codex-ide-terminal--compact-input-background (current-buffer))
    ;; The renderer fits fallback glyphs to Latin font metrics by default.
    ;; Set the minimum locally, for both echoed input and assistant replies.
    (setq-local ghostel-glyph-scale-floor
                codex-ide-terminal-ghostel-glyph-scale-floor)
    (codex-ide-terminal--configure-row-height)
    (when (fboundp 'ghostel--invalidate) (ghostel--invalidate))
    (when (fboundp 'ghostel--adjust-size)
      (dolist (window (get-buffer-window-list (current-buffer) nil t))
        (ghostel--adjust-size window t))))
  (force-mode-line-update))

(define-minor-mode codex-ide-terminal-mode
  "Codex CLI in a terminal, with a live editor-context footer and MCP bridge.
Direct CLI prompts can query the MCP tool get_editor_context.  C-c C-p
sends a prompt with the current editor context explicitly attached."
  :lighter " Codex" :keymap codex-ide-terminal-mode-map
  (when codex-ide-terminal-mode
    (codex-ide-terminal--enable-ghostel-ime)
    ;; Ghostel's semi-char minor map also binds C-c C-p.  Give this
    ;; buffer's Codex commands priority without changing Ghostel globally.
    (setq-local emulation-mode-map-alists
                (cons 'codex-ide-terminal--emulation-map-alist
                      (remq 'codex-ide-terminal--emulation-map-alist
                            emulation-mode-map-alists)))
    (codex-ide-terminal--setup-display)
    (add-hook 'post-command-hook #'codex-ide-terminal--focus nil t)
    (add-hook 'kill-buffer-hook #'codex-ide-terminal--cleanup nil t)))

(defun codex-ide-terminal--send (text &optional submit)
  "Send TEXT to the current Codex terminal, optionally SUBMIT."
  (unless (process-live-p (get-buffer-process (current-buffer)))
    (user-error "This Codex terminal has exited"))
  (when (eq codex-ide-terminal--picker-action 'delete)
    (user-error "Choose a conversation in the delete menu before sending prompts"))
  (pcase codex-ide-terminal--backend
    ('vterm
     (vterm-send-string text t)
     (when submit (vterm-send-return)))
    ('ghostel
     ;; Ghostel's native PTY API expects raw UTF-8 bytes.
     (ghostel-send-string (encode-coding-string (concat "\e[200~" text "\e[201~") 'utf-8-unix))
     (when submit (ghostel-send-string "\r")))
    ('term
     (term-send-raw-string (concat "\e[200~" text "\e[201~"))
     (when submit (term-send-raw-string "\r")))
    (_ (user-error "Unknown terminal backend"))))

(defun codex-ide-terminal-send-prompt (buffer prompt context)
  "Send PROMPT and CONTEXT to terminal BUFFER."
  (with-current-buffer buffer
    (codex-ide-terminal--publish-context)
    (codex-ide-terminal--send (concat prompt context) t)))

(defun codex-ide-terminal-interrupt (buffer)
  "Send Escape to Codex in BUFFER, without closing its terminal."
  (with-current-buffer buffer
    (let ((process (get-buffer-process buffer)))
      (unless (process-live-p process) (user-error "This Codex terminal has exited"))
      ;; Ghostel's lifecycle process can be an event pipe, not its PTY.
      (if (eq codex-ide-terminal--backend 'ghostel)
          (ghostel-send-string "\e")
        (process-send-string process "\e")))))

;;;###autoload
(defun codex-ide-terminal (&optional resume)
  "Open the real Codex CLI in a side terminal with an Emacs MCP context bridge.
With prefix argument RESUME, open the shared conversation picker.
Programmatic RESUME value `delete' opens the same menu for deletion."
  (interactive "P")
  (let* ((root (if resume (codex-ide--resume-project-root) (codex-ide--root)))
         (source (codex-ide--source-buffer))
         (program (or (executable-find codex-ide-executable)
                      (user-error "Codex CLI is not installed")))
         (backend (codex-ide-terminal--choose-backend))
         (name (generate-new-buffer-name
                (format "*codex-ide[%s]*" (file-name-nondirectory (directory-file-name root)))))
         (runtime (make-temp-file "codex-ide-editor-" t))
         (state-file (expand-file-name "editor.json" runtime))
         (buffer nil)
         (layout (current-window-configuration))
         (default-directory root))
    (condition-case err
        (let* ((command (codex-ide-terminal--command program root state-file resume))
               (program (car command))
               (args (cdr command)))
          ;; Write an empty state before Codex can start its MCP subprocess.
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region "{}" nil state-file nil 'silent))
          (set-file-modes state-file #o600)
          ;; Establish the final viewport before the CLI renders its first frame.
          ;; Starting at 80x24 and shrinking afterward can leave a messy prompt.
          (setq buffer (get-buffer-create name))
          (when-let* ((window (display-buffer buffer codex-ide-display-action)))
            (select-window window))
          (pcase backend
            ('ghostel
             (let ((ghostel-kill-buffer-on-exit nil))
               (ghostel-exec buffer program args))
             (with-current-buffer buffer (setq-local ghostel-kill-buffer-on-exit nil)))
            ('vterm
             (let ((vterm-shell (mapconcat #'shell-quote-argument (cons program args) " "))
                   (vterm-kill-buffer-on-exit nil))
               (setq buffer (save-window-excursion (vterm name)))))
            ('term
             (setq buffer (apply #'make-term (substring name 1 -1) program nil args))
             (with-current-buffer buffer (term-char-mode))))
          (with-current-buffer buffer
            (setq-local codex-ide-terminal--backend backend
                        codex-ide-terminal--runtime runtime
                        codex-ide-terminal--state-file state-file
                        codex-ide-terminal--picker-action resume
                        codex-ide--cwd root
                        codex-ide--thread-id (concat "terminal:" name)
                        codex-ide--editor-buffer source)
            (codex-ide-terminal-mode 1)
            (puthash codex-ide--thread-id buffer codex-ide--sessions)
            (setq codex-ide--active-id codex-ide--thread-id)
            (codex-ide-terminal--publish-context)
            (let* ((process (get-buffer-process buffer))
                   (original-sentinel (and process (process-sentinel process))))
              (unless (process-live-p process) (error "Codex exited during terminal startup"))
              (set-process-query-on-exit-flag process nil)
              (set-process-sentinel
               process
               (lambda (proc event)
                 (unwind-protect
                     (when original-sentinel (funcall original-sentinel proc event))
                   (when (and (not (process-live-p proc)) (buffer-live-p buffer))
                     (with-current-buffer buffer (codex-ide-terminal--cleanup))))))))
          (when-let* ((window (display-buffer buffer codex-ide-display-action)))
            (select-window window))
          buffer)
      ((error quit)
       (when (buffer-live-p buffer)
         (when-let* ((process (get-buffer-process buffer))) (delete-process process))
         (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))
       (when (file-directory-p runtime) (delete-directory runtime t))
       (set-window-configuration layout)
       (signal (car err) (cdr err))))))

(add-hook 'codex-ide-context-update-hook #'codex-ide-terminal--schedule-context)
;; Upgrade open terminals without restarting their processes or input methods.
(maphash (lambda (_id buffer)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (bound-and-true-p codex-ide-terminal-mode)
                 (codex-ide-terminal--enable-ghostel-ime)
                 (codex-ide-terminal--setup-display)))))
         codex-ide--sessions)
(provide 'codex-ide-terminal)
;;; codex-ide-terminal.el ends here
