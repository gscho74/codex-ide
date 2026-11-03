(setq default-input-method "korean-hangul")

;; Treat Right Alt / AltGr as a Korean input toggle in PGTK Emacs.
;; pgtk-keysym-table translates these before modifier-only keys are discarded.
(when (boundp 'pgtk-keysym-table)
  (unless (hash-table-p pgtk-keysym-table)
    (setq pgtk-keysym-table (make-hash-table :test 'eql)))
  (puthash #xffea #xe000 pgtk-keysym-table) ; Alt_R
  (puthash #xfe03 #xe000 pgtk-keysym-table) ; ISO_Level3_Shift (AltGr)
  (dolist (modifiers '(nil (meta) (control) (control meta)))
    (define-key key-translation-map
                (vector (event-convert-list (append modifiers (list #xe000))))
                [right-alt-hangul])))
(defun my-right-alt-toggle-input-method ()
  "Toggle input with right Alt and protect Codex from its residual modifier."
  (interactive)
  (toggle-input-method)
  (when (fboundp 'codex-ide-terminal--guard-right-alt)
    (codex-ide-terminal--guard-right-alt)))
(global-set-key [right-alt-hangul] #'my-right-alt-toggle-input-method)
(global-set-key [hangul] #'toggle-input-method)

(set-face-attribute 'default nil :family "Consolas" :height 120)
(add-to-list 'default-frame-alist '(font . "Consolas-12"))

;; Keep native menus at the editor's font family and point size.
;;(defun my-sync-menu-font (&optional frame)
;;  "Match menu text to the default face on FRAME."
;;  (when (display-graphic-p frame)
;;    (set-face-attribute 'menu frame
;;                        :family (face-attribute 'default :family frame)
;;                        :height (face-attribute 'default :height frame)
;;                        :weight (face-attribute 'default :weight frame)
;;                        :slant (face-attribute 'default :slant frame))))
;;(add-hook 'after-make-frame-functions #'my-sync-menu-font)
;;(add-hook 'emacs-startup-hook #'my-sync-menu-font)
;;(my-sync-menu-font)

;; Use a slightly smaller font for native menus (10pt before DPI scaling).
(set-face-attribute 'menu nil :height 100)

;; Prefer Malgun Gothic for Hangul; retain the existing size adjustment.
(defun my-set-korean-font (frame)
  "Fit Hangul to FRAME's Latin baseline without excessive shrinking."
  (when (display-graphic-p frame)
    (with-selected-frame frame
      (let* ((families (font-family-list))
             (family (cond ((member "Malgun Gothic" families) "Malgun Gothic")
                           ((member "D2Coding" families) "D2Coding")
                           (t "Noto Sans Mono CJK KR")))
             (latin (font-info (face-font 'default)))
             (pixels (aref latin 2))
             ;; Never repeat the large reduction required by Noto's metrics.
             (minimum (max 1 (floor (* pixels 0.9))))
             (fits nil))
        (while (not fits)
          (set-fontset-font t 'hangul
                            (font-spec :family family :size pixels) frame)
          (let* ((font (font-at 0 nil "한"))
                 (info (and font (font-info font))))
            (setq fits (or (<= pixels minimum) (null info)
                           (and (<= (aref info 8) (aref latin 8))
                                (<= (aref info 9) (aref latin 9))))))
          (setq pixels (1- pixels)))))))
(add-hook 'after-make-frame-functions #'my-set-korean-font)
(my-set-korean-font (selected-frame))

;;; codex-ide managed startup
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(require 'codex-ide)
(setq codex-ide-interface 'terminal
      codex-ide-terminal-backend 'ghostel)
(codex-ide-context-mode 1)
(global-set-key (kbd "C-c C-o") #'codex-ide-menu)
;;; codex-ide managed startup end
