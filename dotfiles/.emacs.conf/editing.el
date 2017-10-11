;;;; Configuration settings for: Editing Buffers

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; Don't do this, can leak sensitive data.
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Set language environment and input method
(setq current-language-environment "UTF-8")
(setq default-input-method "rfc1345")

;; Set ispell program name
(setq ispell-program-name "aspell")

;; Disable Word-Wrap and set F12 to toggle Word-Wrap
(setq default-truncate-lines t)
(global-set-key [f12] 'toggle-truncate-lines)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Indent with spaces
(setq-default indent-tabs-mode nil)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; Set default indendation width
(setq-default standard-indent 4)

;; Set default tab width
(setq-default tab-width 4)

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; Automatically indent to proper level after every <RET>. t by default.
(setq electric-indent-mode t)

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;;;  These settings relate to how emacs interacts with your operating system

;; makes killing/yanking interact with the clipboard
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

;; Save clipboard strings into kill ring before replacing them.
;; When one selects something in another program to paste it into Emacs,
;; but kills something in Emacs before actually pasting it,
;; this selection is gone unless this variable is non-nil
(setq save-interprogram-paste-before-kill t)

;; Shows all options when running apropos. For more info,
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
(setq apropos-do-all t)

;; Mouse yank commands yank at point instead of at click.
(setq mouse-yank-at-point t)

;; Fills remainder of line with comment start characters
(defun fill-to-end ()
  (interactive)
  (save-excursion
    (end-of-line)
    (while (< (current-column) 70)
      (insert comment-start))))

;; Ensure company mode is disabled globally.
(global-company-mode nil)
(setq company-idle-delay nil)
