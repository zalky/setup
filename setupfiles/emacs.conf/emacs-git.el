;;;; .emacs --- init file for editing git commit messages

;; Copyright (C) Zalan Kemenczy

;; Author:  Zalan Kemenczy <zalan.k@gmail.com>
;; Created: Feb 12, 2007
;; Revised: Mar 2, 2015
;; Emacs v: 24.4.90.1


(add-to-list 'load-path "~/local/share/elisp/")

;; Load word-count-mode
(require 'word-count)

;; "Shrink-wrap" frame to buffer size.
(require 'fit-frame)

;; If "feature" is not already loaded, then it is loaded from either
;; the given filename, or from the filename taken to be feature.el(c).
(require 'info)
(require 'advice)
(require 'cl)


;;;; Load utility functions

;; Function to swap a buffer with the one in a window bellow it. This function
;; is later bound globally to the key `C-x 9'
(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
 
                                     
;;;; Global Key Bindings

;; Global keymaps can be done using `global-set-key', which takes
;; "\key" and 'command as arguments. Mode specific keymaps are bound
;; using the `define-key' function, with the specific keymap, "\key"
;; and 'command as arguments.
(define-key global-map "\C-z" 'undo)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-x 9") 'transpose-buffers)


;;;; Workarounds

;; Fix node.js prompt in eshell
(setenv "NODE_NO_READLINE" "1")

;; Fix junk characters in shell mode (due to loading .bashrc)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; XFT fix for development snapshot
(push '(font-backend xft x) default-frame-alist)

;; This makes path match shell variable and puts anaconda python in search path
(defun set-exec-path-from-shell-PATH ()
        (interactive)
        (let ((path-from-shell (replace-regexp-in-string "^.*\n.*shell\n" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator))))
 
(set-exec-path-from-shell-PATH)


;;;; Enable disabled commands

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Enable horizontal scrolling
(put 'scroll-left 'disabled nil)


;;;; Miscellaneous Settings

;; Hide the toolbar and the menubar
(tool-bar-mode -1)
(menu-bar-mode t)

;; Preserve cursor position when scrolling
(setq-default scroll-preserve-screen-position t)

;; Use spaces only when auto-formating a region
(setq-default indent-tabs-mode nil)

;; Additional space to put between lines when displaying a buffer.
;; The space is measured in pixels...
(setq-default line-spacing 2)

;; ...Except in term-mode, where extra line-spacing messes up expected
;; buffer lengths. Set line-spacing to 0.
(add-hook 'term-mode-hook
          (lambda ()
            (set (make-local-variable 'line-spacing)
                 0)))

;; Set default indendation width
(setq-default standard-indent 4)

;; When Line Number mode is enabled, the line number appears in the
;; mode line
(line-number-mode t)

;; When Column Number mode is enabled, the column number appears in
;; the mode line
(column-number-mode t)

;; Toggle mouse wheel support
(mouse-wheel-mode t)

;; Set non-nil to ignore case when searching
(setq case-fold-search t)

;; Set language environment and input method
(setq current-language-environment "UTF-8")
(setq default-input-method "rfc1345")

;; Highlight matching parenthesis
(show-paren-mode t)

;; Toggle progressive scroll speed (the faster the user moves the
;; wheel, the faster the scrolling). By default the
;; `mouse-wheel-scroll-amount' is 5 (1 with shift held down).
(setq-default mouse-wheel-progressive-speed nil)

;; Set the number of lines of overlap that the `C-v' and `M-v'
;; commands leave
(setq next-screen-context-lines 8)

;; Make a backup of a file the first time it is saved.  This can be
;; done by renaming the file or by copying.
(setq make-backup-files t)

;; Save all backup files to a single directory
(setq backup-directory-alist '(("." . "~/.emacs-backups/")))

;; Inhibit the startup screen. Also inhibits display of the initial
;; message in the *scratch* buffer.
(setq inhibit-startup-message t)

;; Specify whether to have vertical scroll bars, and on which side.
;; Possible values are `nil' (no scroll bars), `left' (scroll bars on
;; left) and `right' (scroll bars on right).
(scroll-bar-mode -1)

;; In Transient Mark mode, the region is highlighted when the mark is
;; set, done in customize.
(transient-mark-mode t)

;; Highlight matching parenthesis, done in customize
(show-paren-mode t)

;; Disable Word-Wrap and set F12 to toggle Word-Wrap
(setq default-truncate-lines t)
(global-set-key [f12] 'toggle-truncate-lines)

;; Disable cursor blink
(blink-cursor-mode nil)

;; Enable Windmove commands to move directionally between windows with
;; `S-right', `S-left', `S-up' and `S-down'.
(windmove-default-keybindings)

;; Change mac command keys
(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)


;;;; Display

;; Define custom face attributes
(setq my-font "Menlo") ;; On Windows: "Consolas"
(setq my-fgcolor "#cccccc")
(setq my-bgcolor "#303030")

;; Set default frame attributes and initial position
(setq my-frame-alist `((width . 90)
		       (height . 30)))

;; You can specify geometry-related options for the initial frame,
;; however they won't take effect until Emacs reads `.emacs', which
;; happens after first creating the frame. Therefore, set the selected
;; frame's position instead.
(setq default-frame-alist (append my-frame-alist default-frame-alist))
(setq initial-frame-alist (append my-frame-alist initial-frame-alist))

(set-frame-position (selected-frame) 400 40)

;; Set face attributes for existing faces (t: default for all frames)
(set-face-attribute 'default t
                    :background my-bgcolor
                    :foreground my-fgcolor
                    :family my-font
                    :height 130)

(set-face-attribute 'variable-pitch t
                    :family my-font)

(set-face-attribute 'fixed-pitch t
                    :family my-font)

(set-face-attribute 'border t
                    :background "#454545")

(set-face-attribute 'vertical-border t
                    :foreground "#454545")

(set-face-attribute 'cursor t
                    :background "#ecdc6b"
                    :foreground "Black")

(set-face-attribute 'fringe t
                    :background my-bgcolor
                    :foreground "#555555")

(set-face-attribute 'highlight t
                    :background "DarkOrange")

(set-face-attribute 'header-line t
                    :inherit 'mode-line
                    :background "#383838"
                    :box nil)

(set-face-attribute 'info-header-xref t
                    :inherit 'info-xref :foreground "LightSteelBlue")

(set-face-attribute 'info-node t
                    :foreground "DeepSkyBlue"
                    :slant 'italic
                    :weight 'bold)

(set-face-attribute 'mouse t
                    :background "Red")

(set-face-attribute 'region t
                    :background "grey30")

(set-face-attribute 'secondary-selection t
                    :background "grey50")

(set-face-attribute 'mode-line t
                    :background "#444444" 
                    :foreground "#909090"
                    :box '(:line-width 1 :color "#454545"))

(set-face-attribute 'mode-line-inactive t
                    :background "#444444"
                    :foreground my-bgcolor
                    :box '(:line-width 1 :color "#454545"))

(set-face-attribute 'mode-line-highlight nil)

(set-face-attribute 'minibuffer-prompt t
                    :weight 'bold
                    :foreground "grey85")

;;;; Major Modes

;; Enable font lock mode for all major modes that support font-lock
(global-font-lock-mode t)

;; Set Text Mode as the default mode with Autofill enabled.
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Set CC mode default styles
(setq c-default-style '((c-mode . "bsd")
                        (c++-mode . "bsd")))

(defun my-c-mode-hook ()
  (setq c-basic-offset 4))
(add-hook 'c-mode-hook 'my-c-mode-hook)

(defun my-c++-mode-hook ()
  (setq c-basic-offset 4))
(add-hook 'c++-mode-hook 'my-c-mode-hook)

(defun my-python-mode-hook ()
  (setq c-basic-offset 4))
(add-hook 'python-mode-hook 'my-python-mode-hook)

(defun white-mode ()
  "Simple Mode to input text."
  (interactive)
  (progn (set-face-attribute 'default (selected-frame)
                             :background "#ffffff"
                             :foreground "#000000")
         (set-face-attribute 'cursor (selected-frame)
                             :background "Grey")
         (set-face-attribute 'fringe (selected-frame)
                             :background "#ffffff")
         (set-face-attribute 'region (selected-frame)
                             :background "#9b9bb7")
         ;; (set-window-fringes (selected-window) 20 20)
         (set-frame-size (selected-frame) 105 (frame-height))
         (setq line-spacing 2)
         (visual-line-mode t)
         (auto-fill-mode -1)
         (flyspell-mode t)
         "Entering Writer Mode"))

(defun writer-mode ()
  "Simple Mode to input text."
  (interactive)
  (progn (set-face-attribute 'default (selected-frame)
                             :background "#ffffff"
                             :foreground "#000000")
         (set-face-attribute 'cursor (selected-frame)
                             :background "Grey")
         (set-face-attribute 'fringe (selected-frame)
                             :background "#ffffff")
         (set-face-attribute 'region (selected-frame)
                             :background "#9b9bb7")
         (set-window-fringes (selected-window) 20 20)
         (set-frame-size (selected-frame) 105 (frame-height))
         (setq line-spacing 20)
         (visual-line-mode t)
         (auto-fill-mode -1)
         (flyspell-mode t)
         ;; Create "Times New Roman-10.5" (= :height 104) overlay
         ;; Or create "Times New Roman-14" (=:heigh 136) overlay
         (setq font-overlay
               (make-overlay (point-min) (point-max) (current-buffer) nil t))
         (overlay-put font-overlay 'face '(:family "Times New Roman" :height 134)) 
         "Entering Writer Mode"))


;;;; Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "gray45" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "gray85" :weight bold)))))
