;;;; Configuration settings for: Editing Buffers

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; Set language environment and input method
(setq current-language-environment "UTF-8")
(setq default-input-method "rfc1345")

;; Disable Word-Wrap and set F12 to toggle Word-Wrap
(setq default-truncate-lines t)
(global-set-key [f12] 'toggle-truncate-lines)

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode t)

;; Highlight current line
(setq-default global-hl-line-mode nil)

;; Use spaces only when auto-formating a region
(setq-default indent-tabs-mode nil)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
;;(global-rainbow-delimiters-mode t)

;; Set default indendation width
(setq-default standard-indent 4)

;; Set default tab width
(setq-default tab-width 4)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width standard-indent)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

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

;; Additional space to put between lines when displaying a buffer.
;; The space is measured in pixels...
(setq-default line-spacing 3)

;; ...Except in term-mode, where extra line-spacing messes up expected
;; buffer lengths. Set line-spacing to 0.
(add-hook 'term-mode-hook
          (lambda ()
            (set (make-local-variable 'line-spacing)
                 0)))

;; These settings relate to how emacs interacts with your operating system
(setq
 ;; makes killing/yanking interact with the clipboard
 x-select-enable-clipboard t

 ;; I'm actually not sure what this does but it's recommended?
 x-select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)
