

;; Author:  Zalan Kemenczy
;; Created: Mon Feb 12, 2007
;; Revised: Mon Feb 23, 2015
;; Emacs v: 24.4.90


;;;; Notes and Reminders

;; HELP: Type `C-h' (`help-command'), and then type a letter to
;; indicate the subject on which you want help. For an explanation on
;; the help facility type `C-h C-h' (`help-for-help').

;; APROPOS: Type `C-h d' (`apropos-documentation') to show symbols
;; whose documentation contains matches for regexp. `info-apropos'
;; grovels indices of all known info files for keywords, and builds a
;; menu of possible matches.

;; DESCRIBE KEY: `C-h c' (`describe-key-briefly') displays the command
;; run by the given key sequence.

;; DESCRIBE VARIABLE: `C-h v' (`describe-variable') displays the full
;; documentation of a given variable. If the variable has a
;; buffer-local value (default to the current buffer), it is displayed
;; along with the global value.

;; DESCRIBE FUNCTION: `C-h f' (`describe-function') displays the full
;; documentation of a given function.

;; MODE HELP: `C-h m' (`describe-mode') finds documentation about the
;; current mode.

;; NARROW: `C-x n n' restricts editing of the buffer to between the
;; point and mark (`narrow-to-region'). `C-x n w' widens to make the
;; entire buffer accessible again (`widen').

;; KILL RECTANGLE: `C-x r k' (`kill-rectangle') kills text within the
;; rectangluar region demarcated by the cursor and the mark.

;; YANK RECTANGLE: `C-x r y' (`yank-rectangle') yanks text within the
;; rectangular region demarcated by the cursor and the mark.

;; COPYING: `M-w' (`kill-ring-save') saves the region as the last
;; killed text without actually killing it.

;; PREFIX & INDENDATION: Type `C-x .' to set the `fill-prefix', and
;; then `C-M-\' (`indent-region') will indent the region to that
;; pattern. To reset the `fill-prefix', simply type `C-x .' at the
;; beginning of a line. Note: numerous editing commands use
;; `fill-prefix' to extend functionality. `M-q' (`fill-paragraph') for
;; instance fills the current paragraph and `C-x f'
;; (`set-fill-column') sets the fill column local to the buffer. Use
;; `M-x fill-region' to fill each paragraph in the region.

;; LISP EVAL: `C-x C-e' (`eval-last-sexp') evaluates the Lisp
;; expression BEFORE the point, and prints the value in the echo
;; area. `M-:' (`eval-expression') reads a single Lisp expression in
;; the minibuffer, evaluates it, and prints the value in the echo
;; area. `eval-region' evaluates all the Lisp expressions in the
;; selected region.  `eval-buffer' evaluates all the Lisp expressions
;; in the current buffer.


;;;; Load Path

;; The `load' command evaluates a complete file, thereby installing
;; all of the functions and variables in that file into Emacs. Rather
;; than load each file explicity, you can specify a directory which
;; Emacs searches when loading a file or function. The `autoload'
;; command makes a function available, but does not evaluate the
;; containing file until the function is actually called.
(add-to-list 'load-path "~/local/share/elisp/")
;;(add-to-list 'load-path "~/local/share/elisp/ecb-2.32")

;;;; Requires

;; Load CEDET
(load "~/.emacs.d/cedet-config.el")

(require 'semantic/sb)

;; Load CEDET
;;(load-file "~/local/share/elisp/cedet-1.0pre4/common/cedet.el")

;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more ideas.
;; Select one of the following:

;; * This enables the database and idle reparse engines
;;(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
;;(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as the nascent intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-guady-code-helpers)

;; * This turns on which-func support (Plus all other code helpers)
;; (semantic-load-enable-excessive-code-helpers)

;; This turns on modes that aid in grammar writing and semantic tool
;; development.  It does not enable any other features such as code
;; helpers above.
;; (semantic-load-enable-semantic-debugging-helpers)

;; Prevent semantic from littering directories with cache files
(setq semanticdb-default-save-directory "~/.semantic/")

;; Require Emacs Code Browser
(add-to-list 'load-path (expand-file-name "~/local/share/elisp/ecb/"))
(require 'ecb)

;; Disable tip of the day
(setq ecb-tip-of-the-day nil)

;; Remove ugly icons from emacs code browser
(setq ecb-tree-buffer-style 'ascii-no-guides)

;; Set primary mouse click to be left click and Ctrl right click
(setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)

;; If "feature" is not already loaded, then it is loaded from either
;; the given filename, or from the filename taken to be feature.el(c).
(require 'info)
(require 'advice)
(require 'cl)

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

;; transpose-frame.el
(require 'transpose-frame)

;; ;; This program provides some interactive functions which allows users
;; to transpose windows arrangement in currently selected frame:
;;
;; `transpose-frame'  ...  Swap x-direction and y-direction
;;
;;        +------------+------------+      +----------------+--------+
;;        |            |     B      |      |        A       |        |
;;        |     A      +------------+      |                |        |
;;        |            |     C      |  =>  +--------+-------+   D    |
;;        +------------+------------+      |   B    |   C   |        |
;;        |            D            |      |        |       |        |
;;        +-------------------------+      +--------+-------+--------+
;;
;; `flip-frame'  ...  Flip vertically
;;
;;        +------------+------------+      +------------+------------+
;;        |            |     B      |      |            D            |
;;        |     A      +------------+      +------------+------------+
;;        |            |     C      |  =>  |            |     C      |
;;        +------------+------------+      |     A      +------------+
;;        |            D            |      |            |     B      |
;;        +-------------------------+      +------------+------------+
;;
;; `flop-frame'  ...  Flop horizontally
;;
;;        +------------+------------+      +------------+------------+
;;        |            |     B      |      |     B      |            |
;;        |     A      +------------+      +------------+     A      |
;;        |            |     C      |  =>  |     C      |            |
;;        +------------+------------+      +------------+------------+
;;        |            D            |      |            D            |
;;        +-------------------------+      +-------------------------+
;;
;; `rotate-frame'  ...  Rotate 180 degrees
;;
;;        +------------+------------+      +-------------------------+
;;        |            |     B      |      |            D            |
;;        |     A      +------------+      +------------+------------+
;;        |            |     C      |  =>  |     C      |            |
;;        +------------+------------+      +------------+     A      |
;;        |            D            |      |     B      |            |
;;        +-------------------------+      +------------+------------+
;;
;; `rotate-frame-clockwise'  ...  Rotate 90 degrees clockwise
;;
;;        +------------+------------+      +-------+-----------------+
;;        |            |     B      |      |       |        A        |
;;        |     A      +------------+      |       |                 |
;;        |            |     C      |  =>  |   D   +--------+--------+
;;        +------------+------------+      |       |   B    |   C    |
;;        |            D            |      |       |        |        |
;;        +-------------------------+      +-------+--------+--------+
;;
;; `rotate-frame-anticlockwise'  ...  Rotate 90 degrees anti-clockwise
;;
;;        +------------+------------+      +--------+--------+-------+
;;        |            |     B      |      |   B    |   C    |       |
;;        |     A      +------------+      |        |        |       |
;;        |            |     C      |  =>  +--------+--------+   D   |
;;        +------------+------------+      |        A        |       |
;;        |            D            |      |                 |       |
;;        +-------------------------+      +-----------------+-------+

;; Load word-count-mode
(require 'word-count)

;; "Shrink-wrap" frame to buffer size.
(require 'fit-frame)
 
;; Fix node.js prompt in eshell
(setenv "NODE_NO_READLINE" "1")

                                     
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

;; Fix junk characters in shell mode (due to loading .bashrc)
;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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
;; The space is measured in pixels.
(setq-default line-spacing 2)

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

;; Set Emacs Code Browser version
(setq ecb-option-version "2.32")

;; Highlight matching parenthesis
(show-paren-mode t)

;; Toggle progressive scroll speed (the faster the user moves the
;; wheel, the faster the scrolling). By default the
;; `mouse-wheel-scroll-amount' is 5 (1 with shift held down).
(setq-default mouse-wheel-progressive-speed nil)

;; Set the number of lines of overlap that the `C-v' and `M-v'
;; commands leave
(setq next-screen-context-lines 8)

;; Toggle use of the tool bar
;; (tool-bar-mode nil) ;; done in customize.

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
(setq my-font "Menlo") ; Alternative: "Courier 10 Pitch-8"
(setq my-fgcolor "#cccccc") ;previous was "#d0d0d0"
(setq my-bgcolor "#303030")

;; Set default frame attributes and initial position
(setq my-frame-alist `((width . 90)
		       (height . 90)))

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
                    :background "#ecdc6b"  ;;"#e3f8fe"
                    :foreground "Black")

(set-face-attribute 'fringe t
                    :background my-bgcolor
                    :foreground "#555555")

(set-face-attribute 'highlight t
                    :background "DarkOrange")

(set-face-attribute 'header-line t
                    :inherit 'mode-line
                    ;;:background "#303030"
                    :background "#383838"
                    :box nil)

;; (set-face-attribute 'tooltip t
;;                     :background my-bgcolor
;;                     :foreground my-fgcolor)

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

;; Fix Ansi color code recognition
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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
                             :background "#ffffff"  ; "#faf0e6"
                             :foreground "#000000")
         (set-face-attribute 'cursor (selected-frame)
                             :background "Grey")
         (set-face-attribute 'fringe (selected-frame)
                             :background "#ffffff")  ; "#faf0e6"
         (set-face-attribute 'region (selected-frame)
                             :background "#9b9bb7") ; ffd799")
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
                             :background "#ffffff"  ; "#faf0e6"
                             :foreground "#000000")
         (set-face-attribute 'cursor (selected-frame)
                             :background "Grey")
         (set-face-attribute 'fringe (selected-frame)
                             :background "#ffffff")  ; "#faf0e6"
         (set-face-attribute 'region (selected-frame)
                             :background "#9b9bb7")  ; ffd799")
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


;;;; Desktop

;; The first time an Emacs session is saved, it must be done manually
;; using `M-x desktop-save'. Save to the directory from which you will
;; start your next emacs session. Emacs will use the .emacs.desktop in
;; the current directory at startup.
(desktop-save-mode t)
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
