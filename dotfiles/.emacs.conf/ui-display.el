;;;; Configuration settings for: User Interface

;; Inhibit the startup screen. Also inhibits display of the initial
;; message in the *scratch* buffer.
(setq inhibit-startup-message t)

;; Hide the toolbar and the menubar
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq linum-format " %5d ")

(fringe-mode '(18 . 18))

;; Show line numbers

;; Do not turn on linum-mode globally
;; (global-linum-mode 0)

;; Prefer per-mode usage instead.
(add-hook 'clojure-mode-hook 'linum-mode)
(add-hook 'lisp-mode-hook 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'python-mode-hook 'linum-mode)
(add-hook 'c-mode-hook 'linum-mode)
(add-hook 'sh-mode-hook 'linum-mode)

;; Additional space to put between lines when displaying a buffer.
;; The space is measured in pixels...
(setq-default line-spacing 0)   ; used to be 6

;; ...Except in term-mode, where extra line-spacing messes up expected          
;; buffer lengths. Set line-spacing to 0.
(add-hook 'term-mode-hook
          (lambda ()
            (set (make-local-variable 'line-spacing)
                 0)))

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

;; Ask for confirmation before quitting Emacs
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          'append)

;; Set non-nil to ignore case when searching with isearch
(setq case-fold-search t)

;; Preserve cursor position when scrolling
(setq-default scroll-preserve-screen-position t)

;; Enable horizontal scrolling
(put 'scroll-left 'disabled nil)

;; Toggle progressive scroll speed (the faster the user moves the
;; wheel, the faster the scrolling). By default the
;; `mouse-wheel-scroll-amount' is 5 (1 with shift held down).
(setq-default mouse-wheel-progressive-speed nil)

;; Set the number of lines of overlap that the `C-v' and `M-v'
;; commands leave
(setq next-screen-context-lines 8)

;; In Transient Mark mode, the region is highlighted when the mark is
;; set, done in customize.
(transient-mark-mode t)

;; Highlight matching parenthesis
(show-paren-mode t)

;; Highlight current line
(setq-default global-hl-line-mode nil)
