;;;; Configuration settings for: User Interface

;; Inhibit the startup screen. Also inhibits display of the initial
;; message in the *scratch* buffer.
(setq inhibit-startup-message t)

;; Hide the toolbar and the menubar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Show line numbers
;;(global-linum-mode 0)

;; Set the column number to appear in the mode line
(column-number-mode t)

;; Specify whether to have vertical scroll bars, and on which side.
;; Possible values are `nil' (no scroll bars), `left' (scroll bars on
;; left) and `right' (scroll bars on right).
(scroll-bar-mode -1)

;; No cursor blinking, it's distracting
(setq-default blink-cursor-mode nil)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;; Toggle mouse wheel support
(mouse-wheel-mode t)

;; Change all 'yes/no' questions to 'y/n'
(fset 'yes-or-no-p 'y-or-n-p)

;; Set 'vertical' window split in ediff by default
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)

;; Disable dialog box prompts
(setq-default use-dialog-box nil)
