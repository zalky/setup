;;;; .emacs --- init file

;; Copyright (C) Zalan Kemenczy

;; Author:  Zalan Kemenczy <zalan.k@gmail.com>
;; Created: Feb 12, 2007
;; Revised: Apr 9, 2015
;; Emacs v: 24.4.91.1


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

;; FILL PARAGRAPH: `M-q' (`fill-paragraph')

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


;;;; Set Loadpath

;; The `load' command evaluates a complete file, thereby installing
;; all of the functions and variables in that file into Emacs. Rather
;; than load each file explicity, you add a directory to the load-path
;; which Emacs searches when loading a file or function. The `autoload'
;; command makes a function available, but does not evaluate the
;; containing file until the function is actually called.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/local/share/elisp/")
(add-to-list 'load-path "~/.emacs.conf/")


;;;; Packages
(load "packages.el")


;;;; Configuration Settings

;; Editing buffers
(load "editing.el")

;; Window management
(load "windows.el")

;; Workflow and convenience tools
(load "projectile-config.el")

;; UI and display configuration
(load "ui-display.el")

;; Org Mode
(load "org-config.el")

;;;; Display
(load "theme.el")

;; Configure helm
(load "helm-my-config.el")

;; Pythong tools
(load "python-config.el")

;; Configure smartparens
(load "smartparens-init.el")

;; Configure tools for editing lisp code
(load "elisp-config.el")

;; Configure tools for editing clojure
(load "clojure-config.el")

;; Configure tools for editing javascript
(load "js-config.el")

;;;; Global Key-Bindings
(load "keybindings.el")


;;;; Workarounds
(load "workarounds.el")


;;;; Major Modes

;; Enable highlighting of numbers as constants for all programming modes
(add-hook 'prog-mode-hook 'highlight-numbers-mode)


;; Set Text Mode as the default mode with Autofill enabled.
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'text-mode-hook-identify)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Configure Octave Mode
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 0)
            (font-lock-mode 1)))

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


;;;; Desktop

;; The first time an Emacs session is saved, it must be done manually
;; using `M-x desktop-save'. Save to the directory from which you will
;; start your next emacs session. Emacs will use the .emacs.desktop in
;; the current directory at startup.
(desktop-save-mode t)

;;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#303030" "#a55684" "#197590" "thistle1" "#0a7bcc" "orchid4" "#607683" "gray80"])
 '(coffee-tab-width 2)
 '(cursor-type t)
 '(global-company-mode nil)
 '(helm-display-header-line nil)
 '(helm-full-frame nil)
 '(helm-prevent-escaping-from-minibuffer nil)
 '(package-selected-packages
   (quote
    (helm-projectile cider-eval-sexp-fu tagedit smex smartparens projectile magit ido-ubiquitous highlight-numbers helm-swoop company clojure-mode-extra-font-locking))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#c0c0c0" :foreground "#303030" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Fira Mono"))))
 '(border ((t (:background "#a9a9a9"))))
 '(cider-debug-code-overlay-face ((t (:inherit highlight))))
 '(cider-fringe-good-face ((t (:inherit highlight))))
 '(cider-repl-prompt-face ((t (:inherit font-lock-comment-face))))
 '(cider-result-overlay-face ((t (:inherit highlight))))
 '(company-scrollbar-bg ((t nil)))
 '(company-scrollbar-fg ((t (:inherit highlight))))
 '(company-tooltip ((t (:inherit region))))
 '(company-tooltip-annotation ((t (:inherit font-lock-comment-face))))
 '(company-tooltip-common ((t (:inherit font-lock-type-face))))
 '(company-tooltip-search ((t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:inherit highlight))))
 '(cursor ((t (:background "HotPink3" :foreground "#303030"))))
 '(diff-added ((t (:inherit (font-lock-variable-name-face diff-added-bg)))))
 '(diff-added-fine-bg ((t (:background "#a9d0b2"))))
 '(diff-changed ((t (:inherit (font-lock-string-face diff-changed-bg)))))
 '(diff-context ((t (:foreground "grey50"))))
 '(diff-refine-added ((t (:inherit (diff-added-fine-bg font-lock-variable-name-face)))))
 '(diff-refine-changed ((t (:inherit (diff-changed-fine-bg font-lock-string-face)))))
 '(diff-refine-removed ((t (:inherit (font-lock-type-face diff-removed-fine-bg)))))
 '(diff-removed ((t (:inherit (font-lock-type-face diff-removed-bg)))))
 '(diff-removed-bg ((t (:background "#c4b5ba"))))
 '(diff-removed-fine-bg ((t (:background "#debfcd"))))
 '(ecb-default-highlight-face ((t (:inherit highlight))))
 '(ecb-mode-line-prefix-face ((t (:inherit font-lock-builtin-face))))
 '(ecb-tag-header-face ((t (:inherit highlight))))
 '(ediff-current-diff-A ((t (:inherit diff-removed-bg))))
 '(ediff-current-diff-B ((t (:inherit diff-added-bg))))
 '(ediff-current-diff-C ((t (:inherit diff-changed-bg))))
 '(ediff-even-diff-A ((t (:inherit region))))
 '(ediff-even-diff-B ((t (:inherit region))))
 '(ediff-even-diff-C ((t (:inherit region))))
 '(ediff-fine-diff-A ((t (:inherit diff-removed-fine-bg))))
 '(ediff-fine-diff-B ((t (:inherit diff-added-fine-bg))))
 '(ediff-fine-diff-C ((t (:inherit diff-changed-fine-bg))))
 '(ediff-odd-diff-A ((t (:inherit region))))
 '(ediff-odd-diff-B ((t (:inherit region))))
 '(ediff-odd-diff-C ((t (:inherit region))))
 '(error ((t (:inherit font-lock-type-face :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "gray30"))))
 '(font-lock-comment-face ((t (:foreground "gray41" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#5e6b94"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((t (:foreground "#2281a9"))))
 '(font-lock-keyword-face ((t (:foreground "grey30" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#817595"))))
 '(font-lock-type-face ((t (:foreground "#a55684"))))
 '(font-lock-variable-name-face ((t (:foreground "#008767"))))
 '(fringe ((t (:background "#c0c0c0" :foreground "#AAAAAA"))))
 '(git-commit-summary ((t (:inherit font-lock-string-face))))
 '(header-line ((t (:background "#b0b0b0" :foreground "#454545" :box nil))))
 '(helm-M-x-key ((t (:inherit link :underline t))))
 '(helm-buffer-directory ((t (:inherit font-lock-function-name-face))))
 '(helm-buffer-not-saved ((t (:inherit font-lock-type-face))))
 '(helm-buffer-process ((t (:inherit font-lock-comment-face))))
 '(helm-buffer-saved-out ((t (:inherit font-lock-variable-name-face))))
 '(helm-candidate-number ((t (:inherit org-mode-line-clock))))
 '(helm-etags-file ((t (:inherit font-lock-string-face))))
 '(helm-ff-directory ((t (:inherit font-lock-function-name-face))))
 '(helm-ff-dotted-directory ((t (:inherit font-lock-builtin-face))))
 '(helm-ff-dotted-symlink-directory ((t (:inherit (font-lock-constant-face highlight)))))
 '(helm-ff-executable ((t (:inherit font-lock-function-name-face))))
 '(helm-ff-file ((t (:inherit default))))
 '(helm-ff-invalid-symlink ((t (:inherit (font-lock-type-face highlight)))))
 '(helm-ff-prefix ((t (:inherit font-lock-keyword-face))))
 '(helm-ff-symlink ((t (:inherit font-lock-constant-face))))
 '(helm-grep-file ((t (:inherit font-lock-builtin-face))))
 '(helm-grep-finish ((t (:inherit font-lock-variable-name-face))))
 '(helm-grep-lineno ((t (:inherit font-lock-comment-face))))
 '(helm-grep-match ((t (:inherit link :underline nil :weight bold))))
 '(helm-grep-running ((t (:inherit font-lock-type-face))))
 '(helm-header ((t (:inherit header-line :slant italic))))
 '(helm-history-remote ((t (:inherit font-lock-string-face))))
 '(helm-lisp-completion-info ((t (:inherit font-lock-type-face))))
 '(helm-lisp-show-completion ((t (:inherit font-lock-variable-name-face))))
 '(helm-match ((t (:inherit ido-only-match))))
 '(helm-match-item ((t (:inherit lazy-highlight))))
 '(helm-moccur-buffer ((t (:inherit font-lock-builtin-face))))
 '(helm-prefarg ((t (:inherit font-lock-builtin-face))))
 '(helm-selection ((t (:inherit header-line))))
 '(helm-selection-line ((t nil)))
 '(helm-separator ((t (:inherit font-lock-type-face))))
 '(helm-source-header ((t (:inherit font-lock-keyword-face :weight bold))))
 '(helm-swoop-line-number-face ((t (:inherit linum))))
 '(helm-swoop-target-line-block-face ((t (:inherit region))))
 '(helm-swoop-target-line-face ((t (:inherit header-line))))
 '(helm-swoop-target-word-face ((t (:inherit lazy-highlight))))
 '(helm-visible-mark ((t (:inherit region))))
 '(highlight ((t (:background "gray70"))))
 '(ido-indicator ((t (:inherit link :underline nil :width condensed))))
 '(ido-only-match ((t (:inherit font-lock-type-face))))
 '(ido-subdir ((t (:inherit font-lock-function-name-face))))
 '(info-header-xref ((t (:inherit info-xref))))
 '(info-node ((t (:inherit link :slant italic :weight bold))))
 '(isearch ((t (:background "hotpink4" :foreground "#b9b9b9"))))
 '(isearch-fail ((t (:inherit isearch))))
 '(ivy-current-match ((t (:inherit highlight))))
 '(ivy-match-required-face ((t (:inherit link :underline nil :weight bold))))
 '(js2-error ((t (:inherit font-lock-warning-face))))
 '(js2-external-variable ((t (:inherit font-lock-variable-name-face))))
 '(js2-function-call ((t (:inherit default))))
 '(js2-function-param ((t (:inherit font-lock-variable-name-face))))
 '(js2-instance-member ((t (:inherit font-lock-variable-name-face))))
 '(js2-private-function-call ((t (:inherit default))))
 '(js2-private-member ((t (:inherit default))))
 '(js2-warning ((t (:inherit font-lock-warning-face))))
 '(lazy-highlight ((t (:background "gray65" :foreground "#c0c0c0"))))
 '(link ((t (:foreground "orchid4" :underline t))))
 '(link-visited ((t (:inherit link :foreground "MediumPurple4"))))
 '(linum ((t (:inherit default :foreground "#a0a0a0"))))
 '(magit-blame-heading ((t (:inherit highlight))))
 '(magit-branch-current ((t (:inherit magit-branch-local))))
 '(magit-branch-local ((t (:inherit font-lock-variable-name-face :weight bold))))
 '(magit-branch-remote ((t (:inherit font-lock-type-face :weight bold))))
 '(magit-cherry-equivalent ((t (:inherit font-lock-string-face))))
 '(magit-diff-added ((t (:inherit diff-added))))
 '(magit-diff-added-highlight ((t (:inherit magit-diff-added))))
 '(magit-diff-base ((t (:inherit diff-changed))))
 '(magit-diff-base-highlight ((t (:inherit diff-changed))))
 '(magit-diff-context ((t (:inherit diff-context))))
 '(magit-diff-context-highlight ((t (:inherit magit-diff-context))))
 '(magit-diff-file-heading-highlight ((t nil)))
 '(magit-diff-hunk-heading ((t nil)))
 '(magit-diff-hunk-heading-highlight ((t nil)))
 '(magit-diff-our ((t (:inherit magit-diff-added))))
 '(magit-diff-our-highlight ((t (:inherit magit-diff-added-highlight))))
 '(magit-diff-removed ((t (:inherit diff-removed))))
 '(magit-diff-removed-highlight ((t (:inherit diff-removed))))
 '(magit-diff-their ((t (:inherit magit-diff-removed))))
 '(magit-diff-their-highlight ((t (:inherit magit-diff-removedt-highlight))))
 '(magit-header-line ((t (:inherit ##))))
 '(magit-log-author ((t (:inherit font-lock-builtin-face))))
 '(magit-section-heading ((t (:underline t :weight bold))))
 '(magit-section-highlight ((t nil)))
 '(magit-tag ((t (:inherit font-lock-constant-face))))
 '(match ((t (:inherit (link highlight) :underline nil :weight bold))))
 '(mmm-default-submode-face ((t nil)))
 '(mode-line ((t (:background "#a0a0a0" :foreground "#595959" :box (:line-width 1 :color "#a0a0a0")))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#a0a0a0" :foreground "#c0c0c0" :box (:line-width 1 :color "#a0a0a0") :weight light))))
 '(org-agenda-done ((t (:foreground "PaleGreen"))))
 '(org-column ((t (:background "grey63" :strike-through nil :underline nil :slant normal :weight normal))))
 '(org-column-title ((t (:background "grey63" :underline t :weight bold))))
 '(org-date ((t (:foreground "gray45" :underline t))))
 '(org-done ((t (:foreground "gray50" :weight bold))))
 '(org-hide ((t (:foreground "#b9b9b9"))))
 '(org-level-1 ((t (:inherit outline-1 :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :weight bold))))
 '(org-level-3 ((t (:inherit font-lock-type-face :weight bold))))
 '(org-level-4 ((t (:inherit link :underline nil :weight bold))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "SkyBlue4" :weight bold))))
 '(org-level-6 ((t (:inherit outline-6 :weight bold))))
 '(org-level-7 ((t (:inherit outline-7 :weight bold))))
 '(org-level-8 ((t (:inherit outline-8 :weight bold))))
 '(org-mode-line-clock ((t (:inherit mode-line :foreground "gray78"))) t)
 '(org-mode-line-clock-overrun ((t (:inherit mode-line :foreground "gray78" :weight bold))) t)
 '(org-special-keyword ((t (:inherit font-lock-comment-face :foreground "gray50" :slant italic :weight normal))))
 '(org-table ((t (:inherit default))))
 '(org-todo ((t (:foreground "gray40" :weight bold))))
 '(region ((t (:background "grey79"))))
 '(region-light ((t (:background "grey79"))))
 '(semantic-highlight-func-current-tag-face ((t (:inherit highlight))))
 '(sh-heredoc ((t (:inherit font-lock-string-face))))
 '(sh-quoted-exec ((t (:inherit font-lock-builtin-face))))
 '(show-paren-match ((t (:background "#999999" :foreground "#c0c0c0"))))
 '(show-paren-mismatch ((t (:background "PaleVioletRed4" :foreground "#c0c0c0"))))
 '(smerge-base ((t (:inherit diff-changed))))
 '(smerge-mine ((t (:inherit diff-removed))))
 '(smerge-other ((t (:inherit diff-added))))
 '(smerge-refined-added ((t (:inherit diff-added))))
 '(smerge-refined-removed ((t (:inherit diff-removed))))
 '(speedbar-tag-face ((t (:inherit link :underline nil))))
 '(swiper-line-face ((t (:inherit highlight))))
 '(swiper-match-face-2 ((t (:inherit isearch-lazy-highlight-face))))
 '(swiper-match-face-3 ((t (:inherit isearch-lazy-highlight-faec))))
 '(swiper-match-face-4 ((t (:inherit isearch-lazy-highlight-face))))
 '(vertical-border ((t (:foreground "#a9a9a9"))))
 '(wgrep-face ((t (:inherit diff-added-bg))))
 '(wgrep-reject-face ((t (:inherit font-lock-warning-face)))))

