;;; codex-ide.el --- Codex App Server IDE client for Emacs -*- lexical-binding: t; -*-
;; Version: 0.5.0
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: MIT
;;; Commentary:
;; Independent App Server JSON-RPC client. Does not modify claude-code-ide.
;; Current-buffer and selected-line context is gathered at every send.
;; Approval requests always require user confirmation and are never auto-granted.
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'project)
(require 'subr-x)
(require 'diff)
(require 'seq)
(require 'vc)
(require 'thingatpt)
(require 'flymake)
(defvar codex-ide-context-mode)
(defgroup codex-ide nil "Codex App Server integration." :group 'tools)
(defcustom codex-ide-executable "codex" "Codex CLI program." :type 'string)
(defcustom codex-ide-sandbox "read-only" "Sandbox mode." :type '(choice (const "read-only") (const "workspace-write")))
(defcustom codex-ide-context-max-chars 12000 "Maximum source selection characters to send." :type 'integer)
(defcustom codex-ide-display-action '((display-buffer-reuse-window display-buffer-in-side-window) (side . right) (window-width . 0.43)) "Buffer display action." :type 'sexp)
(defcustom codex-ide-interface 'app-server
  "UI opened by `codex-ide'.  Terminal runs the real Codex CLI.
App-server uses the native chat composer and Emacs approval/Ediff workflow."
  :type '(choice (const app-server) (const terminal)))
(declare-function codex-ide-terminal "codex-ide-terminal" (&optional resume))
(declare-function codex-ide-terminal-send-prompt "codex-ide-terminal" (buffer prompt context))
(declare-function codex-ide-terminal-interrupt "codex-ide-terminal" (buffer))
(defvar codex-ide--process nil)
(defvar codex-ide--pending (make-hash-table :test 'equal))
(defvar codex-ide--proposed-items (make-hash-table :test 'equal))
(defvar codex-ide--approval-history nil)
(defvar codex-ide--request-id 0)
(defvar codex-ide--partial "")
(defvar codex-ide--initialized nil)
(defvar codex-ide--sessions (make-hash-table :test 'equal))
(defvar codex-ide--active-id nil)
(defvar codex-ide--source nil)
(defvar codex-ide--snapshots nil)
(defvar codex-ide--last-changed nil)
(defvar codex-ide--snapshots-dir nil)
(defvar codex-ide--selection-state nil)
(defvar codex-ide--last-source-state nil)
(defcustom codex-ide-snapshot-limit 400 "Maximum files per snapshot; large projects should use Git for exhaustive review." :type 'integer)
(defcustom codex-ide-file-size-limit 2000000 "Maximum bytes for a file snapshot." :type 'integer)
(defcustom codex-ide-diagnostics-limit 30 "Maximum diagnostics included in context." :type 'integer)
(defcustom codex-ide-auto-context t "Update active file/region live in Emacs; transmit only when a prompt is sent." :type 'boolean)
(defcustom codex-ide-include-diagnostics t "Include current Emacs diagnostic errors and warnings in prompts." :type 'boolean)
(defvar codex-ide--snapshot-root nil)
(defvar codex-ide--debug "*codex-ide-log*")
(defvar-local codex-ide--thread-id nil)
(defvar-local codex-ide--cwd nil)
(defvar-local codex-ide--busy nil)
(defvar-local codex-ide--last-turn nil)
(defvar-local codex-ide--delta-start nil)
(defvar-local codex-ide--input-start nil)
(defvar-local codex-ide--output-end nil)
(defvar-local codex-ide--input-history nil)
(defvar-local codex-ide--history-index nil)
(defvar-local codex-ide--input-draft "")
(defvar-local codex-ide--editor-buffer nil)
(defvar-local codex-ide--tracked-region nil)
(defvar-local codex-ide--dismissed-context nil)
(defvar-local codex-ide--context-overlay nil)
(defvar codex-ide-context-update-hook nil
  "Hook run in each session buffer when its editor context changes.")
(defun codex-ide--log (fmt &rest args)
  (with-current-buffer (get-buffer-create codex-ide--debug)
    (goto-char (point-max)) (insert (format-time-string "%T ") (apply #'format fmt args) "\n")))
(defun codex-ide--get (object key) (and (hash-table-p object) (gethash key object)))
(defun codex-ide--obj (&rest pairs)
  (let ((h (make-hash-table :test 'equal)))
    (while pairs (puthash (pop pairs) (pop pairs) h)) h))
(defun codex-ide--root (&optional b)
  (with-current-buffer (or b (current-buffer))
    (expand-file-name (or (when-let* ((p (project-current nil))) (project-root p)) default-directory))))
(defun codex-ide--append (b s)
  (when (and (buffer-live-p b)
             (not (with-current-buffer b (bound-and-true-p codex-ide-terminal-mode))))
    (with-current-buffer b
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (if (markerp codex-ide--output-end)
                         codex-ide--output-end (point-max)))
          (insert (if (markerp codex-ide--output-end)
                      (propertize s 'read-only t 'front-sticky '(read-only)
                                  'rear-nonsticky t)
                    s)))))))
(defun codex-ide--send-raw (obj)
  (unless (process-live-p codex-ide--process) (user-error "Codex App Server is not running"))
  (process-send-string codex-ide--process (concat (json-serialize obj :null-object nil :false-object :json-false) "\n")))
(defun codex-ide--notify (method params)
  (codex-ide--send-raw (codex-ide--obj "method" method "params" params)))
(defun codex-ide--request (method params callback &optional error-callback)
  (cl-incf codex-ide--request-id)
  (puthash codex-ide--request-id (cons callback error-callback) codex-ide--pending)
  (codex-ide--send-raw (codex-ide--obj "id" codex-ide--request-id "method" method "params" params)))
(defun codex-ide--response (id result &optional error)
  (codex-ide--send-raw (if error (codex-ide--obj "id" id "error" error)
                         (codex-ide--obj "id" id "result" result))))
(defun codex-ide--proposed-key (thread-id item-id)
  (cons thread-id item-id))

(defun codex-ide--remember-proposal (params)
  "Remember proposed file changes before server requests approval."
  (let* ((item (codex-ide--get params "item"))
         (item-id (codex-ide--get item "id"))
         (thread-id (codex-ide--get params "threadId")))
    (when (and (equal (codex-ide--get item "type") "fileChange") item-id)
      (puthash (codex-ide--proposed-key thread-id item-id)
               (codex-ide--get item "changes") codex-ide--proposed-items))))

(defun codex-ide--approval-preview (method params)
  "Display all proposed file diffs in a read-only diff-mode buffer."
  (let* ((changes (gethash (codex-ide--proposed-key
                           (codex-ide--get params "threadId")
                           (codex-ide--get params "itemId"))
                          codex-ide--proposed-items))
         (buffer (get-buffer-create "*codex-ide-approval-preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Codex request: %s\nThread: %s\nReason: %s\n\n"
                        method (codex-ide--get params "threadId")
                        (or (codex-ide--get params "reason") "(none)")))
        (if (and (equal method "item/fileChange/requestApproval") changes)
            (dolist (change changes)
              (insert (format "\n===== %s [%s] =====\n"
                              (or (codex-ide--get change "path") "(unknown)")
                              (or (codex-ide--get change "kind") "unknown")))
              (insert (or (codex-ide--get change "diff")
                          "No diff supplied by App Server; cannot inspect proposed contents."))
              (insert "\n"))
          (insert (format "Command: %S\nCWD: %S\n"
                          (codex-ide--get params "command")
                          (codex-ide--get params "cwd")))
          (when (equal method "item/fileChange/requestApproval")
            (insert "\nWARNING: No proposed diff was supplied. Decline unless verified separately.\n")))
        (diff-mode)))
    (display-buffer buffer '(display-buffer-pop-up-window))
    (cons buffer changes)))

(defun codex-ide--review-proposed-ediff (change &optional project-root)
  "Compare patch proposal for CHANGE against a temporary copy, not project files.
A unified diff must apply cleanly. Otherwise signal an error and do not approve."
  (let* ((path (codex-ide--get change "path"))
         (patch (codex-ide--get change "diff"))
         (root (or project-root codex-ide--snapshot-root
                   (when-let* ((b (codex-ide--buffer-for codex-ide--active-id)))
                     (buffer-local-value 'codex-ide--cwd b))
                   default-directory))
         (source (and path (expand-file-name path root))))
    (unless (and source (string-prefix-p (file-name-as-directory (file-truename root))
                                          (file-name-directory (file-truename source)))
                 (stringp patch) (executable-find "patch"))
      (user-error "Cannot safely preview patch for %s" path))
    (let* ((old (make-temp-file "codex-proposed-old-"))
           (new (make-temp-file "codex-proposed-new-"))
           (patch-file (make-temp-file "codex-proposed-diff-"))
           (status nil))
      (when (file-exists-p source) (copy-file source old t))
      (copy-file old new t)
      (with-temp-file patch-file
        ;; App Server can supply hunks without unified diff file headers.
        (unless (string-match-p "^--- " patch)
          (insert "--- old\n+++ new\n"))
        (insert patch))
      (setq status (call-process "patch" nil nil nil "--batch" "--forward"
                                 "--fuzz=0" "--no-backup-if-mismatch"
                                 new patch-file))
      (delete-file patch-file)
      (unless (eq status 0)
        (delete-file old) (delete-file new)
        (user-error "Proposed patch cannot be applied safely for preview: %s" path))
      (require 'ediff)
      (unwind-protect
          (let ((control (ediff-files old new)))
            (with-current-buffer control
              (setq-local ediff-after-quit-hook-internal
                          (cons (lambda () (when (> (recursion-depth) 0)
                                             (exit-recursive-edit)))
                                ediff-after-quit-hook-internal)))
            (message "Review proposal; quit Ediff with q to return to approval")
            (recursive-edit))
        (dolist (file (list old new))
          (when-let* ((buffer (get-file-buffer file)))
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer))
          (when (file-exists-p file) (delete-file file)))))))

(defun codex-ide--review-approval (method params)
  "Inspect proposed changes and return explicit decision. Never auto-approve."
  (let* ((preview (codex-ide--approval-preview method params))
         (session (codex-ide--buffer-for (codex-ide--get params "threadId")))
         (project-root (when (buffer-live-p session)
                         (buffer-local-value 'codex-ide--cwd session)))
         (changes (cdr preview))
         (file-change (equal method "item/fileChange/requestApproval"))
         (has-diffs (and changes
                         (cl-every (lambda (c)
                                     (let ((diff (codex-ide--get c "diff")))
                                       (and (stringp diff) (not (string-blank-p diff))))) changes)))
         (choice (read-char-choice
                  (cond
                   ((and file-change (not has-diffs)) "Missing proposed diff: [d]ecline: ")
                   (file-change
                      (format "Codex proposes %d file change(s): [a]pprove [d]ecline [e]diff selected: " (length changes)))
                    (t "Codex command/permission: [a]pprove [d]ecline: "))
                  (cond ((not file-change) '(?a ?d)) (has-diffs '(?a ?d ?e)) (t '(?d))))))
    (when (and file-change (not has-diffs)) (setq choice ?d))
    (while (eq choice ?e)
      (let* ((names (mapcar (lambda (c) (cons (codex-ide--get c "path") c)) changes))
             (selected (cdr (assoc (completing-read "Ediff proposed file: " names nil t) names))))
        (condition-case err (codex-ide--review-proposed-ediff selected project-root)
          (error (message "Ediff preview unavailable: %s" (error-message-string err)))))
      (setq choice (read-char-choice "After reviewing: [a]pprove [d]ecline [e]diff again: " '(?a ?d ?e))))
    (if (eq choice ?a) "accept" "decline")))

(defun codex-ide--handle-approval (message)
  "Handle App Server initiated request, denying unsupported methods."
  (let ((id (codex-ide--get message "id"))
        (method (codex-ide--get message "method"))
        (params (codex-ide--get message "params"))
        (server codex-ide--process))
    (run-at-time
     0 nil
     (lambda ()
       (let* ((supported (member method '("item/fileChange/requestApproval"
                                          "item/commandExecution/requestApproval")))
              (decision (if supported
                            (condition-case err (codex-ide--review-approval method params)
                              (quit "decline")
                              (error (codex-ide--log "Approval UI: %s" err) "decline"))
                          "decline")))
         (push (list method id decision) codex-ide--approval-history)
         (when (and (eq server codex-ide--process) (process-live-p server))
           (if supported
               (codex-ide--response id (codex-ide--obj "decision" decision))
             (codex-ide--response id nil
                                  (codex-ide--obj "code" -32601
                                                  "message" "Unsupported server request")))))))))

(defun codex-ide--handle-message (m)
  (let ((id (codex-ide--get m "id")) (method (codex-ide--get m "method"))
        (params (codex-ide--get m "params")))
    (cond
     ((and id (not method))
      (let* ((handlers (gethash id codex-ide--pending))
             (cb (car-safe handlers)) (error-cb (cdr-safe handlers)))
        (remhash id codex-ide--pending)
        (if (codex-ide--get m "error")
            (progn (codex-ide--log "RPC error %s: %S" id (codex-ide--get m "error"))
                   (message "Codex RPC error: %s" (codex-ide--get (codex-ide--get m "error") "message"))
                   (when error-cb (funcall error-cb (codex-ide--get m "error"))))
          (when cb (funcall cb (codex-ide--get m "result"))))))
     ((and id method) (codex-ide--handle-approval m))
     (method (codex-ide--handle-event method params)))))
(defun codex-ide--filter (_process chunk)
  (setq codex-ide--partial (concat codex-ide--partial chunk))
  (let (line pos)
    (while (setq pos (string-match "\n" codex-ide--partial))
      (setq line (substring codex-ide--partial 0 pos)
            codex-ide--partial (substring codex-ide--partial (1+ pos)))
      (unless (string-empty-p (string-trim line))
        (condition-case err
            (codex-ide--handle-message (json-parse-string line :object-type 'hash-table :array-type 'list :null-object nil :false-object :json-false))
          (error (codex-ide--log "Parse/dispatch: %s; %.300s" err line)))))))
(defun codex-ide--sentinel (process event)
  (when (eq process codex-ide--process)
  (setq codex-ide--initialized nil)
  (maphash (lambda (_id handlers)
             (when-let* ((callback (cdr-safe handlers)))
               (condition-case err
                   (funcall callback (codex-ide--obj "message" "App Server disconnected"))
                 (error (codex-ide--log "Disconnect callback: %s" err)))))
           codex-ide--pending)
  (clrhash codex-ide--pending)
  (clrhash codex-ide--proposed-items)
  (maphash (lambda (_id buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (setq codex-ide--busy nil))))
           codex-ide--sessions)
  (codex-ide--log "Server exited: %s" (string-trim event))))
(defun codex-ide--ensure-server (callback)
  (if (and (process-live-p codex-ide--process) codex-ide--initialized)
      (funcall callback)
    (unless (executable-find codex-ide-executable) (user-error "Codex CLI missing in this WSL environment"))
    (unless (process-live-p codex-ide--process)
      (setq codex-ide--partial "" codex-ide--pending (make-hash-table :test 'equal))
      (let ((default-directory (expand-file-name "~/")))
        (setq codex-ide--process
              (make-process :name "codex-ide-app-server" :buffer (get-buffer-create " *codex-ide-stderr*")
                            :command (list codex-ide-executable "app-server" "--listen" "stdio://")
                            :connection-type 'pipe :coding 'utf-8-unix :noquery t
                            :filter #'codex-ide--filter :sentinel #'codex-ide--sentinel
                            :stderr (get-buffer-create "*codex-ide-stderr*")))))
    (codex-ide--request "initialize"
                        (codex-ide--obj "clientInfo" (codex-ide--obj "name" "emacs_codex_ide" "title" "Emacs Codex IDE" "version" "0.5.0")
                                         "capabilities" (codex-ide--obj))
                        (lambda (_res)
                          (codex-ide--notify "initialized" (codex-ide--obj))
                          (setq codex-ide--initialized t)
                          (funcall callback))
                        (lambda (err)
                          (codex-ide--log "Initialize failed: %S" err)
                          (codex-ide--stop-app-server)))))
(defun codex-ide--buffer-for (id) (gethash id codex-ide--sessions))
(defun codex-ide--activate (thread cwd)
  (let* ((id (codex-ide--get thread "id"))
         (b (or (codex-ide--buffer-for id)
                (get-buffer-create (format "*codex-ide:%s*" (substring id 0 (min 8 (length id))))))))
    (with-current-buffer b
      (unless (derived-mode-p 'codex-ide-chat-mode) (codex-ide-chat-mode))
      (setq-local codex-ide--thread-id id codex-ide--cwd cwd)
      (setq-local default-directory cwd)
      (codex-ide--update-context-display))
    (puthash id b codex-ide--sessions)
    (setq codex-ide--active-id id)
    (codex-ide--append b (format "\nCodex · %s\n\n"
                                 (file-name-nondirectory (directory-file-name cwd))))
    (select-window (display-buffer b codex-ide-display-action))
    (goto-char (point-max))
    b))
(defun codex-ide--active-buffer ()
  (or (and (or (derived-mode-p 'codex-ide-chat-mode)
               (bound-and-true-p codex-ide-terminal-mode)) (current-buffer))
      (codex-ide--buffer-for codex-ide--active-id)
      (user-error "Start a Codex thread with M-x codex-ide")))
(defun codex-ide--source-buffer ()
  (if (and buffer-file-name (not (derived-mode-p 'codex-ide-chat-mode))) (current-buffer)
    (let* ((session (or (and codex-ide--thread-id (current-buffer))
                        (codex-ide--buffer-for codex-ide--active-id)))
           (local-source (and (buffer-live-p session)
                              (buffer-local-value 'codex-ide--editor-buffer session)))
           (source (if (buffer-live-p local-source) local-source codex-ide--source))
           (cwd (and (buffer-live-p session) (buffer-local-value 'codex-ide--cwd session))))
      (when (and (buffer-live-p source)
                 (or (not cwd) (codex-ide--source-in-root-p source cwd)))
        source))))

(defun codex-ide--source-in-root-p (source root)
  "Whether SOURCE visits a file within ROOT."
  (when-let* ((file (buffer-file-name source)))
    (string-prefix-p (file-name-as-directory (expand-file-name root))
                     (expand-file-name file))))

(defun codex-ide--region-bounds ()
  "Return the live selection or its tracked markers in the source buffer."
  (cond ((use-region-p) (cons (region-beginning) (region-end)))
        ((and codex-ide--tracked-region
              (marker-position (car codex-ide--tracked-region))
              (marker-position (cdr codex-ide--tracked-region))
              (< (car codex-ide--tracked-region) (cdr codex-ide--tracked-region)))
         (cons (marker-position (car codex-ide--tracked-region))
               (marker-position (cdr codex-ide--tracked-region))))))

(defun codex-ide--context-fingerprint (source)
  "Identify SOURCE and its selection, independently of cursor movement."
  (with-current-buffer source
    (list buffer-file-name (codex-ide--region-bounds))))

(defun codex-ide--context-source ()
  "Return the source unless its context was dismissed in this session."
  (let* ((session (or (and codex-ide--thread-id (current-buffer))
                      (codex-ide--buffer-for codex-ide--active-id)))
         (dismissed (and (buffer-live-p session)
                         (buffer-local-value 'codex-ide--dismissed-context session)))
         (source (codex-ide--source-buffer)))
    (unless (or (not codex-ide-auto-context)
                (and source (equal dismissed (codex-ide--context-fingerprint source))))
      source)))

(defun codex-ide--context-label ()
  "Describe exactly the editor context available to the current session."
  (if-let* ((source (codex-ide--context-source)))
      (with-current-buffer source
        (if-let* ((bounds (codex-ide--region-bounds)))
            (format "⧉ %s · L%d–L%d selected"
                    (file-name-nondirectory buffer-file-name)
                    (line-number-at-pos (car bounds))
                    (line-number-at-pos (max (car bounds) (1- (cdr bounds)))))
          (format "⧉ In %s · L%d" (file-name-nondirectory buffer-file-name)
                  (line-number-at-pos))))
    (if codex-ide--dismissed-context "⧉ Context cleared" "⧉ No editor context")))

(defun codex-ide--update-context-display ()
  "Refresh the context row beside this session's composer."
  (when (and (markerp codex-ide--output-end)
             (marker-buffer codex-ide--output-end))
    (unless (overlayp codex-ide--context-overlay)
      (setq codex-ide--context-overlay
            (make-overlay codex-ide--output-end codex-ide--output-end nil t t)))
    (move-overlay codex-ide--context-overlay codex-ide--output-end codex-ide--output-end)
    (overlay-put codex-ide--context-overlay 'before-string
                 (concat "\n" (propertize (codex-ide--context-label) 'face 'font-lock-comment-face)
                         "\n")))
  (run-hooks 'codex-ide-context-update-hook)
  (force-mode-line-update))
(defun codex-ide--diagnostics (buffer)
  "Extract Flymake/Flycheck diagnostics for BUFFER."
  (with-current-buffer buffer
    (let (rows)
      (when (and codex-ide-include-diagnostics (bound-and-true-p flymake-mode)
                 (fboundp 'flymake-diagnostics))
        (dolist (d (ignore-errors (flymake-diagnostics)))
          (push (format "Flymake L%d: %s" (line-number-at-pos (flymake-diagnostic-beg d))
                        (flymake-diagnostic-text d)) rows)))
      (when (and codex-ide-include-diagnostics (bound-and-true-p flycheck-mode)
                 (boundp 'flycheck-current-errors))
        (dolist (e (symbol-value 'flycheck-current-errors))
          (push (format "Flycheck L%s: %s" (if (fboundp 'flycheck-error-line)
                                                     (flycheck-error-line e) "?")
                        (if (fboundp 'flycheck-error-message)
                            (flycheck-error-message e) "")) rows)))
      (seq-take (nreverse rows) codex-ide-diagnostics-limit))))
(defun codex-ide--context ()
  "Capture precise current editor position, selection and diagnostics."
  (if-let* ((b (codex-ide--context-source)))
      (with-current-buffer b
        (let* ((file (or (buffer-file-name) (buffer-name)))
               (bounds (codex-ide--region-bounds))
               (start (line-number-at-pos (if bounds (car bounds) (point))))
               (end (line-number-at-pos (if bounds (max (car bounds) (1- (cdr bounds))) (point))))
               (selection (when bounds
                            (buffer-substring-no-properties (car bounds)
                                                            (min (cdr bounds) (+ (car bounds) codex-ide-context-max-chars)))))
               (diagnostics (codex-ide--diagnostics b)))
          (concat "\n\n[Emacs editor context]\nFile: " file
                  "\nLines: " (number-to-string start) "-" (number-to-string end)
                  (format "\nCursor column: %d\nSymbol at point: %s\nLanguage server: %s\nUnsaved changes: %s" (current-column) (or (thing-at-point 'symbol t) "none") (if (or (bound-and-true-p eglot--managed-mode) (bound-and-true-p lsp-mode)) "active" "inactive") (if (buffer-modified-p) "yes" "no"))
                  (if selection (concat "\nSelected source:\n```\n" selection "\n```") "")
                  (if diagnostics (concat "\nEditor diagnostics:\n" (string-join diagnostics "\n")) "")
                  "\n[/Emacs editor context]")))
    ""))
(defun codex-ide--track ()
  "Remember the latest editing buffer and active selection."
  (when (and codex-ide-auto-context buffer-file-name (not (minibufferp)))
    (setq codex-ide--source (current-buffer))
    (when codex-ide--tracked-region
      (set-marker (car codex-ide--tracked-region) nil)
      (set-marker (cdr codex-ide--tracked-region) nil))
    (setq codex-ide--tracked-region
          (when (use-region-p)
            (cons (copy-marker (region-beginning)) (copy-marker (region-end) t))))
    (setq codex-ide--last-source-state
          (list (buffer-file-name) (line-number-at-pos)
                (when (use-region-p) (line-number-at-pos (region-beginning)))
                (when (use-region-p) (line-number-at-pos (max (region-beginning) (1- (region-end)))))))
    (let ((source (current-buffer))
          (fingerprint (codex-ide--context-fingerprint (current-buffer))))
      (maphash (lambda (_id buffer)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when (or (not codex-ide--cwd)
                               (codex-ide--source-in-root-p source codex-ide--cwd))
                       (setq codex-ide--editor-buffer source)
                       (unless (equal codex-ide--dismissed-context fingerprint)
                         (setq codex-ide--dismissed-context nil))
                       (codex-ide--update-context-display)))))
               codex-ide--sessions))
    (force-mode-line-update t)))
(defun codex-ide--context-indicator ()
  (if (and codex-ide-context-mode codex-ide--last-source-state)
      (format " Codex[%s:%s%s]"
              (file-name-nondirectory (or (nth 0 codex-ide--last-source-state) ""))
              (or (nth 1 codex-ide--last-source-state) "?")
              (if (nth 2 codex-ide--last-source-state)
                  (format " sel:%s-%s" (nth 2 codex-ide--last-source-state)
                          (nth 3 codex-ide--last-source-state)) "")) ""))
(define-minor-mode codex-ide-context-mode
  "Track the active file and line selection; send context only on prompts."
  :global t :lighter (:eval (codex-ide--context-indicator))
  (if codex-ide-context-mode
      (progn (add-hook 'post-command-hook #'codex-ide--track)
             (add-hook 'buffer-list-update-hook #'codex-ide--track)
             (codex-ide--track))
    (remove-hook 'post-command-hook #'codex-ide--track)
    (remove-hook 'buffer-list-update-hook #'codex-ide--track)
    (setq codex-ide--source nil codex-ide--last-source-state nil)
    (maphash (lambda (_id buffer)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq codex-ide--editor-buffer nil)
                   (codex-ide--update-context-display)))) codex-ide--sessions)))
(defun codex-ide--enable-chat-input ()
  "Protect the transcript and add an editable composer in this chat buffer.
Existing sessions can be upgraded without resetting their thread or draft."
  (setq buffer-read-only nil)
  (add-hook 'post-command-hook #'codex-ide--chat-focus nil t)
  (setq-local truncate-lines nil)
  (setq-local header-line-format
              '((:eval (if codex-ide--busy " Codex: replying" " Codex: ready"))
                " | RET: send · C-j: newline"))
  (visual-line-mode 1)
  (unless (and (markerp codex-ide--input-start)
               (eq (marker-buffer codex-ide--input-start) (current-buffer)))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (or (bobp) (bolp)) (insert "\n"))
      (add-text-properties (point-min) (point-max)
                           '(read-only t front-sticky (read-only) rear-nonsticky t))
      (let ((output-position (point)))
        (insert (propertize "\nYou> " 'read-only t 'front-sticky '(read-only)
                            'rear-nonsticky t))
        (setq codex-ide--output-end (copy-marker output-position t)
              codex-ide--input-start (copy-marker (point))))))
  (codex-ide--update-context-display))

(defun codex-ide--chat-focus ()
  "Keep editor commands and the context row aligned with the visible session."
  (when codex-ide--thread-id (setq codex-ide--active-id codex-ide--thread-id))
  (codex-ide--update-context-display))

(defvar codex-ide-chat-mode-map (make-sparse-keymap))
;; Rebind on reload too, including buffers created by the transcript-only mode.
(set-keymap-parent codex-ide-chat-mode-map text-mode-map)
(dolist (binding '(("RET" . codex-ide-chat-send)
                   ("C-c C-c" . codex-ide-chat-send)
                   ("C-j" . codex-ide-chat-newline)
                   ("<S-return>" . codex-ide-chat-newline)
                   ("M-p" . codex-ide-chat-previous-input)
                   ("M-n" . codex-ide-chat-next-input)
                   ("C-c C-k" . codex-ide-interrupt)
                   ("C-c C-x" . codex-ide-clear-selection)
                   ("C-c C-o" . codex-ide-menu)))
  (define-key codex-ide-chat-mode-map (kbd (car binding)) (cdr binding)))
(define-key codex-ide-chat-mode-map [remap undo] #'codex-ide-chat-undo)
(define-key codex-ide-chat-mode-map [remap undo-only] #'codex-ide-chat-undo)

(define-derived-mode codex-ide-chat-mode text-mode "Codex-IDE"
  "Chat with Codex directly below the protected transcript.
Type after You> and press RET to send.  C-j inserts a newline.
M-p/M-n recall sent messages.  C-c C-k interrupts the current turn."
  (codex-ide--enable-chat-input))

(defun codex-ide-chat-newline ()
  "Insert a newline in the composer, moving there from the transcript."
  (interactive)
  (when (< (point) codex-ide--input-start) (goto-char (point-max)))
  (newline))

(defun codex-ide-chat-undo (&optional count)
  "Undo COUNT composer changes without undoing the conversation."
  (interactive "p")
  (let ((transient-mark-mode t)
        ;; Always rebuild a selective undo list for the current composer.
        (last-command nil))
    (save-mark-and-excursion
      ;; Include the immutable prompt's final space so an empty composer
      ;; never triggers Emacs's whole-buffer fallback for an empty region.
      (goto-char (1- codex-ide--input-start))
      (set-mark (point-max))
      (setq mark-active t)
      (undo-only count))))

(defun codex-ide-chat-send ()
  "Send this chat buffer's composer, preserving the draft on local errors."
  (interactive)
  (unless (derived-mode-p 'codex-ide-chat-mode)
    (user-error "Use this command in a Codex chat buffer"))
  (let ((prompt (buffer-substring-no-properties codex-ide--input-start (point-max))))
    (when (string-empty-p (string-trim prompt)) (user-error "Type a message after You>"))
    (codex-ide-send-prompt prompt)
    (push prompt codex-ide--input-history)
    (setq codex-ide--history-index nil codex-ide--input-draft "")
    (delete-region codex-ide--input-start (point-max))
    (goto-char (point-max))))

(defun codex-ide--recall-input (step)
  "Move STEP entries through this session's input history."
  (unless codex-ide--input-history (user-error "No sent messages yet"))
  (let ((index (+ (or codex-ide--history-index -1) step)))
    (unless (<= -1 index (1- (length codex-ide--input-history)))
      (user-error "No more input history"))
    (unless codex-ide--history-index
      (setq codex-ide--input-draft
            (buffer-substring-no-properties codex-ide--input-start (point-max))))
    (delete-region codex-ide--input-start (point-max))
    (goto-char (point-max))
    (insert (if (= index -1) codex-ide--input-draft
              (nth index codex-ide--input-history)))
    (setq codex-ide--history-index (unless (= index -1) index))))

(defun codex-ide-chat-previous-input ()
  "Recall the previous sent message in this session."
  (interactive)
  (codex-ide--recall-input 1))

(defun codex-ide-chat-next-input ()
  "Recall the next sent message or restore the current draft."
  (interactive)
  (codex-ide--recall-input -1))
(defun codex-ide--project-files (root)
  "Find project files, preferring Git tracking, without touching remote paths."
  (let ((default-directory root))
    (if (and (executable-find "git")
             (file-directory-p (expand-file-name ".git" root)))
        (let ((files (condition-case nil
                         (process-lines "git" "ls-files" "--cached" "--others" "--exclude-standard")
                       (error nil))))
          (mapcar (lambda (f) (expand-file-name f root)) files))
      (when-let* ((b (codex-ide--source-buffer)) (file (buffer-file-name b)))
        (list file)))))
(defun codex-ide--snapshot ()
  "Snapshot source project files before an agent turn, not only the active file."
  (setq codex-ide--snapshots nil codex-ide--last-changed nil)
  (let* ((b (codex-ide--source-buffer))
         (root (when b (codex-ide--root b))))
    (when (and root (file-directory-p root) (not (file-remote-p root)))
      (setq codex-ide--snapshot-root root)
      (when (and codex-ide--snapshots-dir (file-directory-p codex-ide--snapshots-dir))
        (delete-directory codex-ide--snapshots-dir t))
      (setq codex-ide--snapshots-dir (make-temp-file "codex-ide-snapshots-" t))
      (let ((files (seq-take (delete-dups (codex-ide--project-files root)) codex-ide-snapshot-limit)))
        (dolist (file files)
          (when (and (file-regular-p file)
                     (<= (file-attribute-size (file-attributes file)) codex-ide-file-size-limit))
            (let ((copy (expand-file-name (format "%s" (length codex-ide--snapshots)) codex-ide--snapshots-dir)))
              (copy-file file copy t)
              (push (cons file copy) codex-ide--snapshots))))))))
(defun codex-ide--file-hash (file)
  "Hash FILE contents, rather than its path string."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))
(defun codex-ide--finish-snapshot ()
  "Compute all modified or deleted snapshotted files, and newly created files."
  (setq codex-ide--last-changed
        (cl-remove-if-not
         (lambda (pair) (or (not (file-exists-p (car pair)))
                            (not (string= (codex-ide--file-hash (car pair))
                                          (codex-ide--file-hash (cdr pair))))))
         codex-ide--snapshots))
  (when (and codex-ide--snapshot-root codex-ide--snapshots-dir)
    (let ((tracked (mapcar #'car codex-ide--snapshots)))
      (dolist (file (seq-take (codex-ide--project-files codex-ide--snapshot-root)
                              codex-ide-snapshot-limit))
        (when (and (file-regular-p file) (not (member file tracked)))
          (push (cons file nil) codex-ide--last-changed)))))
  (when codex-ide--last-changed
    (message "Codex changed %d files; M-x codex-ide-review-changes" (length codex-ide--last-changed))))
(defun codex-ide--handle-event (method params)
  (let* ((id (codex-ide--get params "threadId"))
         (b (or (and id (codex-ide--buffer-for id)) (codex-ide--buffer-for codex-ide--active-id))))
    (pcase method
      ("item/agentMessage/delta" (codex-ide--append b (or (codex-ide--get params "delta") "")))
      ("item/fileChange/patchUpdated"
       (puthash (codex-ide--proposed-key id (codex-ide--get params "itemId"))
                (codex-ide--get params "changes") codex-ide--proposed-items))
      ("item/started"
       (codex-ide--remember-proposal params)
       (let ((item (codex-ide--get params "item")))
         (when (member (codex-ide--get item "type") '("commandExecution" "fileChange" "mcpToolCall"))
           (codex-ide--append b (format "\n[Tool] %s\n" (codex-ide--get item "type"))))))
      ("turn/diff/updated" (codex-ide--append b "\n[Diff updated]\n"))
      ("turn/completed"
       (when (buffer-live-p b) (with-current-buffer b (setq codex-ide--busy nil)))
       (codex-ide--append b (format "\n\n[Turn: %s]\n" (or (codex-ide--get (codex-ide--get params "turn") "status") "ended")))
       (codex-ide--finish-snapshot))
      ("error" (codex-ide--append b (format "\n[Error] %S\n" params))))))
;;;###autoload
(defun codex-ide ()
  "Start Codex for this project using `codex-ide-interface'."
  (interactive)
  (if (eq codex-ide-interface 'terminal)
      (codex-ide-terminal)
    (codex-ide-app-server)))
;;;###autoload
(defun codex-ide-app-server () "Start a native Codex chat with Emacs Ediff approvals." (interactive)
  (let ((cwd (codex-ide--root)))
    (codex-ide--ensure-server
     (lambda ()
       (codex-ide--request "thread/start" (codex-ide--obj "cwd" cwd "sandbox" codex-ide-sandbox "approvalPolicy" "on-request")
                           (lambda (res) (codex-ide--activate (codex-ide--get res "thread") cwd)))))))
;;;###autoload
(defun codex-ide-send-prompt (prompt)
  "Send PROMPT with file/region context to the active Codex UI."
  (interactive "sCodex: ")
  (let ((buffer (codex-ide--active-buffer)))
    (if (with-current-buffer buffer (bound-and-true-p codex-ide-terminal-mode))
        (codex-ide-terminal-send-prompt buffer prompt (codex-ide--context))
      (codex-ide--send-app-server-prompt prompt))))

(defun codex-ide--send-app-server-prompt (prompt)
  "Send PROMPT and editor context to the active App Server thread."
  (let* ((b (codex-ide--active-buffer))
         (id (buffer-local-value 'codex-ide--thread-id b))
         (context (codex-ide--context))
         (body (concat prompt context)))
    (with-current-buffer b (when codex-ide--busy (user-error "Codex turn is still active"))
      (setq codex-ide--busy t))
    (condition-case err
        (progn
          (codex-ide--snapshot)
          (codex-ide--append b (concat "\nYou: " prompt (if (string-empty-p context) "" " [editor context attached]") "\nCodex: "))
          (codex-ide--request "turn/start" (codex-ide--obj "threadId" id "input" (vector (codex-ide--obj "type" "text" "text" body)))
                        (lambda (res) (when (buffer-live-p b)
                                        (with-current-buffer b (setq codex-ide--last-turn (codex-ide--get (codex-ide--get res "turn") "id")))))
                        (lambda (err)
                          (when (buffer-live-p b)
                            (with-current-buffer b (setq codex-ide--busy nil)))
                          (codex-ide--append b (format "\n[Turn failed: %s]\n" (codex-ide--get err "message"))))))
      (error
       (when (buffer-live-p b) (with-current-buffer b (setq codex-ide--busy nil)))
       (codex-ide--append b (format "\n[Send failed: %s]\n" (error-message-string err)))
       (signal (car err) (cdr err))))))
;;;###autoload
(defun codex-ide-send-region (question)
  "Send a question with selected lines." (interactive "sQuestion about selected lines: ")
  (unless (use-region-p) (user-error "Select code first"))
  (codex-ide-send-prompt (if (string-empty-p question) "Review selected code" question)))
;;;###autoload
(defun codex-ide-insert-at-mentioned ()
  "Display current file and line reference, for prompt composition." (interactive)
  (let ((ctx (codex-ide--context)))
    (kill-new ctx) (message "File and selection context copied; will also be included with next prompt")))
;;;###autoload
(defun codex-ide-clear-selection ()
  "Dismiss this session's file/selection until a new file or region is tracked."
  (interactive)
  (let ((source (codex-ide--source-buffer))
        (session (codex-ide--active-buffer)))
    (with-current-buffer session
      (setq codex-ide--dismissed-context
            (if source (codex-ide--context-fingerprint source) '(cleared)))
      (codex-ide--update-context-display))))
(defun codex-ide--resume-project-root ()
  "Return the canonical project root for a conversation picker.
In a Codex buffer, keep its saved project even if its terminal changed cwd."
  (file-name-as-directory
   (file-truename
    (if (and (stringp codex-ide--cwd) (not (string-empty-p codex-ide--cwd))
             (or (derived-mode-p 'codex-ide-chat-mode)
                 (bound-and-true-p codex-ide-terminal-mode)))
        codex-ide--cwd
      (codex-ide--root)))))

(defun codex-ide--thread-in-project-p (thread root)
  "Whether THREAD's saved cwd denotes the canonical project ROOT."
  (let ((cwd (codex-ide--get thread "cwd")))
    (and (stringp cwd) (file-name-absolute-p cwd) (not (file-remote-p cwd))
         (condition-case nil
             (equal (file-name-as-directory (file-truename cwd)) root)
           (file-error nil)))))

;;;###autoload
(defun codex-ide-resume ()
  "Choose a conversation from the current project and resume it.
Terminal UI opens the Codex CLI's interactive conversation picker in a
new terminal, like `claude-code-ide-resume'."
  (interactive)
  (if (or (eq codex-ide-interface 'terminal)
          (bound-and-true-p codex-ide-terminal-mode))
      (codex-ide-terminal t)
    (codex-ide--resume-app-server)))

(defun codex-ide--resume-app-server ()
  "Select a current-project thread in the native App Server UI."
  (codex-ide--choose-project-conversation
   "Resume project conversation: "
   (lambda (thread root)
     (codex-ide--request
      "thread/resume" (codex-ide--obj "threadId" (codex-ide--get thread "id"))
      (lambda (result) (codex-ide--activate (codex-ide--get result "thread") root))))))

(defun codex-ide--choose-project-conversation (prompt callback)
  "Choose a current-project conversation with PROMPT, then call CALLBACK."
  (let ((root (codex-ide--resume-project-root)))
    (codex-ide--ensure-server
     (lambda ()
       (codex-ide--request
        "thread/list" (codex-ide--obj "limit" 100 "cwd" (vector (directory-file-name root) root))
        (lambda (res)
          (let* ((items (seq-filter (lambda (thread) (codex-ide--thread-in-project-p thread root))
                                    (codex-ide--get res "data")))
                 (choices (mapcar (lambda (thread)
                                    (cons (format "%s | %s | %s"
                                                  (or (codex-ide--get thread "preview") "(no preview)")
                                                  (codex-ide--get thread "cwd")
                                                  (codex-ide--get thread "id")) thread)) items)))
            (if (null choices)
                (message "No saved Codex conversations in %s" (abbreviate-file-name root))
              (let ((picked (cdr (assoc (completing-read prompt choices nil t)
                                        choices))))
                (when picked
                  (funcall callback picked root)))))))))))
;;;###autoload
(defun codex-ide-continue () "Resume through thread selector." (interactive) (codex-ide-resume))
;;;###autoload
(defun codex-ide-list-sessions () "Show stored threads selector." (interactive) (codex-ide-resume))
;;;###autoload
(defun codex-ide-archive (id) "Archive thread ID via App Server." (interactive "sThread ID to archive: ")
  (when (yes-or-no-p (format "Archive %s? " id))
    (codex-ide--request "thread/archive" (codex-ide--obj "threadId" id) (lambda (_) (message "Archived %s" id)))))
;;;###autoload
(defun codex-ide-delete (&optional id)
  "Choose and permanently delete a conversation from the current project.
Use the same menu as `codex-ide-resume' and confirm the selected deletion.
Optional programmatic ID is also checked against the current project."
  (interactive)
  (cond
   (id
    (let ((root (codex-ide--resume-project-root)))
      (codex-ide--ensure-server
       (lambda ()
         (codex-ide--request
          "thread/read" (codex-ide--obj "threadId" id "includeTurns" :json-false)
          (lambda (result)
            (let ((thread (codex-ide--get result "thread")))
              (if (equal id (codex-ide--get thread "id"))
                  (codex-ide--delete-project-conversation thread root)
                (message "Conversation ID did not match; nothing deleted")))))))))
   ((or (eq codex-ide-interface 'terminal) (bound-and-true-p codex-ide-terminal-mode))
    (codex-ide-terminal 'delete))
   (t (codex-ide--choose-project-conversation
       "Delete project conversation: " #'codex-ide--delete-project-conversation))))

(defun codex-ide--delete-project-conversation (thread root)
  "Confirm and delete THREAD only if it still belongs to ROOT."
  (let ((id (codex-ide--get thread "id")))
    (if (not (and (stringp id) (codex-ide--thread-in-project-p thread root)))
        (message "Conversation is outside this project; nothing deleted")
      (when (yes-or-no-p
             (format "Permanently delete %s? "
                     (truncate-string-to-width
                      (replace-regexp-in-string "[\n\r\t]+" " "
                                                (or (codex-ide--get thread "preview") id)) 100)))
        (codex-ide--request
         "thread/read" (codex-ide--obj "threadId" id "includeTurns" :json-false)
         (lambda (result)
           (let ((current (codex-ide--get result "thread")))
             (if (not (and (equal id (codex-ide--get current "id"))
                           (codex-ide--thread-in-project-p current root)))
                 (message "Conversation project changed; nothing deleted")
               (codex-ide--request
                "thread/delete" (codex-ide--obj "threadId" id)
                (lambda (_) (message "Deleted conversation %s" id))
                (lambda (error) (message "Deletion failed: %s" (codex-ide--get error "message"))))))))))))
;;;###autoload
(defun codex-ide-unarchive (id) "Unarchive thread via App Server." (interactive "sThread ID to unarchive: ")
  (codex-ide--request "thread/unarchive" (codex-ide--obj "threadId" id) (lambda (_) (message "Unarchived %s" id))))
;;;###autoload
(defun codex-ide--review-file (pair)
  "Show Ediff for changed PAIR (FILE . SNAPSHOT)."
  (require 'ediff)
  (let* ((file (car pair)) (snapshot (cdr pair))
         (empty (unless (and snapshot (file-exists-p snapshot))
                  (make-temp-file "codex-ide-empty-")))
         (old (or snapshot empty))
         (new (if (file-exists-p file) file
                (or empty (make-temp-file "codex-ide-deleted-")))))
    (ediff-files old new)))
;;;###autoload
(defun codex-ide-review-changes ()
  "Select any changed project file and view before/after with Ediff."
  (interactive)
  (unless codex-ide--last-changed (user-error "No detected changed files"))
  (let* ((choices (mapcar (lambda (pair)
                            (cons (file-relative-name (car pair) (or codex-ide--snapshot-root default-directory)) pair))
                          codex-ide--last-changed))
         (choice (completing-read "Review changed file: " choices nil t)))
    (codex-ide--review-file (cdr (assoc choice choices)))))
;;;###autoload
(defun codex-ide-list-changes ()
  "Show all files touched by the last turn."
  (interactive)
  (if codex-ide--last-changed
      (with-current-buffer (get-buffer-create "*codex-ide-changes*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Changed files (%d):\n\n" (length codex-ide--last-changed)))
          (dolist (pair codex-ide--last-changed)
            (insert (format "%s  %s\n" (cond ((null (cdr pair)) "NEW")
                                                ((not (file-exists-p (car pair))) "DELETED")
                                                (t "MODIFIED")) (car pair))))
          (special-mode))
        (pop-to-buffer (current-buffer)))
    (message "No detected changes")))
;;;###autoload
(defun codex-ide-interrupt () "Interrupt active turn." (interactive)
  (let ((b (codex-ide--active-buffer)))
    (if (with-current-buffer b (bound-and-true-p codex-ide-terminal-mode))
        (codex-ide-terminal-interrupt b)
      (codex-ide--request "turn/interrupt" (codex-ide--obj "threadId" (buffer-local-value 'codex-ide--thread-id b)
                                            "turnId" (or (buffer-local-value 'codex-ide--last-turn b)
                                                         (user-error "No active turn ID")))
                        (lambda (_) (message "Interrupted"))))))
;;;###autoload
(defun codex-ide-switch-to-buffer () "Open active Codex chat." (interactive) (pop-to-buffer (codex-ide--active-buffer)))
;;;###autoload
(defun codex-ide-toggle () "Toggle chat window." (interactive)
  (let* ((b (codex-ide--active-buffer)) (w (get-buffer-window b)))
    (if w (delete-window w) (display-buffer b codex-ide-display-action))))
;;;###autoload
(defun codex-ide-toggle-recent () "Toggle recent chat window." (interactive) (codex-ide-toggle))
;;;###autoload
(defun codex-ide-show-all () "Display active chat buffers." (interactive)
  (maphash (lambda (_id b) (when (buffer-live-p b) (display-buffer b))) codex-ide--sessions))
;;;###autoload
(defun codex-ide--stop-app-server ()
  "Stop the shared App Server without erasing thread history."
  (when (process-live-p codex-ide--process) (delete-process codex-ide--process)) (setq codex-ide--initialized nil))
(defun codex-ide-stop () "Stop the current terminal or shared App Server." (interactive)
  (let ((buffer (or (and (bound-and-true-p codex-ide-terminal-mode) (current-buffer))
                    (codex-ide--buffer-for codex-ide--active-id))))
    (if (and (buffer-live-p buffer)
             (with-current-buffer buffer (bound-and-true-p codex-ide-terminal-mode)))
        (when-let* ((process (get-buffer-process buffer))) (delete-process process))
      (codex-ide--stop-app-server))))
;;;###autoload
(defun codex-ide-stop-all () "Stop Codex terminals and the shared App Server." (interactive)
  (let (processes)
    (maphash (lambda (_id buffer)
               (when (and (buffer-live-p buffer)
                          (with-current-buffer buffer (bound-and-true-p codex-ide-terminal-mode)))
                 (when-let* ((process (get-buffer-process buffer))) (push process processes))))
             codex-ide--sessions)
    (mapc #'delete-process processes))
  (codex-ide--stop-app-server))
;;;###autoload
(defun codex-ide-integration-check ()
  "Check CLI binary, account authentication and App Server availability in WSL."
  (interactive)
  (let ((buffer (get-buffer-create "*codex-ide-integration-check*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "OS: %s\nCLI path: %s\n" system-type
                      (or (executable-find codex-ide-executable) "NOT FOUND")))
      (dolist (args '(("--version") ("login" "status") ("app-server" "--help")))
        (insert (format "\n$ codex %s\n" (string-join args " ")))
        (let ((status (condition-case err
                          (apply #'call-process codex-ide-executable nil buffer t args)
                        (error (insert (error-message-string err)) -1))))
          (insert (format "\nExit code: %s\n" status))))
      (insert "\nFor interactive session/approval testing use test-wsl.sh and ERT.\n")
      (special-mode))
    (pop-to-buffer buffer)))
;;;###autoload
(defun codex-ide-check-status () "Show server status." (interactive)
  (message "CLI=%s server=%s initialized=%s" (or (executable-find codex-ide-executable) "MISSING")
           (process-live-p codex-ide--process) codex-ide--initialized))
;;;###autoload
(defun codex-ide-show-debug () "Show logs." (interactive) (pop-to-buffer (get-buffer-create codex-ide--debug)))
;;;###autoload
(defun codex-ide-clear-debug () "Clear logs." (interactive) (with-current-buffer (get-buffer-create codex-ide--debug) (erase-buffer)))
;;;###autoload
(defun codex-ide-menu () "Choose Codex IDE command." (interactive)
  (let* ((choices '(("Start new" . codex-ide) ("Terminal UI" . codex-ide-terminal)
                    ("Native chat (Ediff)" . codex-ide-app-server) ("Prompt" . codex-ide-send-prompt)
                    ("Clear context" . codex-ide-clear-selection)
                    ("Send region" . codex-ide-send-region) ("Resume" . codex-ide-resume)
                    ("Show chat" . codex-ide-switch-to-buffer) ("Toggle" . codex-ide-toggle)
                    ("Review Ediff" . codex-ide-review-changes) ("List changed files" . codex-ide-list-changes) ("Interrupt" . codex-ide-interrupt)
                    ("Archive" . codex-ide-archive) ("Delete" . codex-ide-delete)
                    ("Status" . codex-ide-check-status) ("WSL diagnostics" . codex-ide-integration-check)))
         (choice (completing-read "Codex IDE: " choices nil t)))
    (call-interactively (cdr (assoc choice choices)))))
(defun codex-ide-refresh-chat-buffers ()
  "Enable inline input in existing sessions after loading an updated package."
  (interactive)
  (maphash (lambda (_id buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (derived-mode-p 'codex-ide-chat-mode)
                   (use-local-map codex-ide-chat-mode-map)
                   (codex-ide--enable-chat-input)))))
           codex-ide--sessions))

(codex-ide-refresh-chat-buffers)
(provide 'codex-ide)
(let ((terminal-file (expand-file-name "codex-ide-terminal.el"
                                       (file-name-directory (or load-file-name buffer-file-name)))))
  ;; Loading by absolute path must also load the matching terminal adapter,
  ;; even when the project is not on load-path or an older copy is loaded.
  (when (file-readable-p terminal-file) (load terminal-file nil t)))
;;; codex-ide.el ends here
