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

;; Configure tools for editing lisp code
(load "elisp-config.el")

;; Configure tools for editing clojure
(load "clojure-config.el")

;; Configure smartparens
(load "smartparens-init.el")

;; Configure paredit
;; (load "paredit-config.el")

;; Configure tools for editing javascript
(load "js-config.el")

;;;; Global Key-Bindings
(load "global-keybindings.el")

;;;; Backup functionality
(load "backup.el")

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
   ["#303030" "#C17BD2" "#00B0A6" "orchid2" "#00ADD7" "#897FBD" "#698BC4" "gray80"])
 '(css-fontify-colors nil)
 '(cursor-type t)
 '(ns-alternate-modifier (quote alt))
 '(ns-right-command-modifier (quote control))
 '(package-selected-packages
   (quote
    (boon cider cider-decompile ac-cider smartparens conda sparql-mode nlinum magit highlight-numbers fit-frame tagedit vue-mode indium helm-projectile helm-swoop helm clj-refactor clojure-mode-extra-font-locking clojure-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3B3B3D" :foreground "#959595" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "nil" :family "Fira Mono"))))
 '(border ((t (:foreground "#363638"))))
 '(cider-debug-code-overlay-face ((t (:inherit highlight))))
 '(cider-error-highlight-face ((t (:inherit nil :underline (:color "DeepPink2" :style wave)))))
 '(cider-fringe-good-face ((t (:inherit highlight))))
 '(cider-reader-conditional-face ((t (:inherit font-lock-comment-face :slant normal))))
 '(cider-repl-prompt-face ((t (:inherit font-lock-comment-face))))
 '(cider-result-overlay-face ((t (:inherit highlight))))
 '(cider-stacktrace-face ((t (:inherit region))))
 '(company-scrollbar-bg ((t nil)))
 '(company-scrollbar-fg ((t (:inherit highlight))))
 '(company-tooltip ((t (:inherit region))))
 '(company-tooltip-annotation ((t (:inherit font-lock-comment-face))))
 '(company-tooltip-common ((t (:inherit font-lock-function-name-face))))
 '(company-tooltip-search ((t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:inherit highlight))))
 '(compilation-info ((t (:inherit font-lock-comment-face))))
 '(css-property ((t (:inherit font-lock-variable-name-face))))
 '(cursor ((t (:inherit default :background "#F97BE3"))))
 '(custom-button ((t (:inherit mode-line :box (:line-width 2 :style released-button)))))
 '(custom-group-tag ((t (:inherit font-lock-keyword-face :weight bold :height 1.2))))
 '(custom-state ((t (:inherit font-lock-string-face))))
 '(custom-variable-tag ((t (:inherit font-lock-keyword-face))))
 '(diff-added ((t (:inherit diff-added-bg))))
 '(diff-added-bg ((t (:background "#4d5452"))))
 '(diff-added-fine-bg ((t (:background "#5a6763"))))
 '(diff-changed ((t (:inherit (font-lock-string-face diff-changed-bg)))))
 '(diff-changed-bg ((t (:background "#474555"))))
 '(diff-changed-fine-bg ((t (:background "#545266"))))
 '(diff-context ((t (:foreground "grey50"))))
 '(diff-file-header ((t (:inherit magit-diff-file-heading))))
 '(diff-header ((t (:inherit magit-diff-file-heading))))
 '(diff-refine-added ((t (:inherit (diff-added-fine-bg font-lock-constant-face)))))
 '(diff-refine-changed ((t (:inherit (diff-changed-fine-bg font-lock-string-face)))))
 '(diff-refine-removed ((t (:inherit (font-lock-function-name-face diff-removed-fine-bg)))))
 '(diff-removed ((t (:inherit diff-removed-bg :underline nil))))
 '(diff-removed-bg ((t (:background "#544d51"))))
 '(diff-removed-fine-bg ((t (:background "#675a65"))))
 '(dired-marked ((t (:inherit font-lock-constant-face :weight bold))))
 '(ecb-default-highlight-face ((t (:inherit highlight))))
 '(ecb-mode-line-prefix-face ((t (:inherit font-lock-builtin-face))))
 '(ecb-tag-header-face ((t (:inherit highlight))))
 '(ediff-current-diff-A ((t (:inherit diff-removed-bg))))
 '(ediff-current-diff-B ((t (:inherit diff-added-bg))))
 '(ediff-current-diff-C ((t (:inherit diff-changed-bg))))
 '(ediff-even-diff-A ((t (:inherit highlight))))
 '(ediff-even-diff-B ((t (:inherit highlight))))
 '(ediff-even-diff-C ((t (:inherit highlight))))
 '(ediff-fine-diff-A ((t (:inherit diff-removed-fine-bg))))
 '(ediff-fine-diff-B ((t (:inherit diff-added-fine-bg))))
 '(ediff-fine-diff-C ((t (:inherit diff-changed-fine-bg))))
 '(ediff-odd-diff-A ((t (:inherit highlight))))
 '(ediff-odd-diff-B ((t (:inherit highlight))))
 '(ediff-odd-diff-C ((t (:inherit highlight))))
 '(error ((t (:inherit font-lock-function-name-face :weight bold))))
 '(eshell-ls-backup ((t (:inherit font-lock-builtin-face))))
 '(eshell-ls-directory ((t (:inherit font-lock-keyword-face :weight bold))))
 '(eshell-ls-executable ((t (:inherit font-lock-type-face))))
 '(eshell-ls-readonly ((t (:inherit font-lock-builtin-face))))
 '(eshell-ls-symlink ((t (:inherit font-lock-constant-face :weight bold))))
 '(eshell-prompt ((t (:inherit font-lock-comment-face))))
 '(flyspell-duplicate ((t (:underline (:color "orchid4" :style wave)))))
 '(flyspell-incorrect ((t (:underline (:color "orchid2" :style wave)))))
 '(font-lock-builtin-face ((t (:foreground "#808080"))))
 '(font-lock-comment-face ((t (:foreground "#656A6D" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#9289AC"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-function-name-face ((t (:foreground "#9e69bf"))))
 '(font-lock-keyword-face ((t (:foreground "#858585" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#839667"))))
 '(font-lock-type-face ((t (:foreground "#AB82C1"))))
 '(font-lock-variable-name-face ((t (:foreground "#62a583"))))
 '(font-lock-warning-face ((t (:inherit error))))
 '(fringe ((t (:inherit (font-lock-comment-face default) :slant normal))))
 '(git-commit-summary ((t (:inherit font-lock-string-face))))
 '(header-line ((t (:background "#38383a" :box nil))))
 '(helm-M-x-key ((t (:inherit link :underline t))))
 '(helm-buffer-directory ((t (:inherit font-lock-type-face))))
 '(helm-buffer-not-saved ((t (:inherit font-lock-function-name-face))))
 '(helm-buffer-process ((t (:inherit font-lock-comment-face))))
 '(helm-buffer-saved-out ((t (:inherit font-lock-constant-face))))
 '(helm-buffer-size ((t (:inherit font-lock-constant-face))))
 '(helm-candidate-number ((t (:inherit org-mode-line-clock))))
 '(helm-etags-file ((t (:inherit font-lock-string-face))))
 '(helm-ff-directory ((t (:inherit font-lock-type-face))))
 '(helm-ff-dotted-directory ((t (:inherit font-lock-builtin-face))))
 '(helm-ff-dotted-symlink-directory ((t (:inherit (font-lock-variable-name-face highlight)))))
 '(helm-ff-executable ((t (:inherit font-lock-type-face))))
 '(helm-ff-file ((t (:inherit default))))
 '(helm-ff-invalid-symlink ((t (:inherit (font-lock-function-name-face highlight)))))
 '(helm-ff-prefix ((t (:inherit font-lock-keyword-face))))
 '(helm-ff-symlink ((t (:inherit font-lock-variable-name-face))))
 '(helm-grep-file ((t (:inherit font-lock-builtin-face))))
 '(helm-grep-finish ((t (:inherit font-lock-constant-face))))
 '(helm-grep-lineno ((t (:inherit font-lock-comment-face))))
 '(helm-grep-match ((t (:inherit link :underline nil :weight bold))))
 '(helm-grep-running ((t (:inherit font-lock-function-name-face))))
 '(helm-header ((t (:inherit header-line :slant italic))))
 '(helm-history-remote ((t (:inherit font-lock-string-face))))
 '(helm-lisp-completion-info ((t (:inherit font-lock-function-name-face))))
 '(helm-lisp-show-completion ((t (:inherit font-lock-constant-face))))
 '(helm-match ((t (:inherit font-lock-type-face))))
 '(helm-match-item ((t (:inherit lazy-highlight))))
 '(helm-moccur-buffer ((t (:inherit font-lock-builtin-face))))
 '(helm-mode-prefix ((t (:inherit region))))
 '(helm-prefarg ((t (:inherit font-lock-builtin-face))))
 '(helm-selection ((t (:inherit nil :background "#343436"))))
 '(helm-selection-line ((t nil)))
 '(helm-separator ((t (:inherit font-lock-comment-face))))
 '(helm-source-header ((t (:inherit font-lock-keyword-face :weight bold))))
 '(helm-swoop-line-number-face ((t (:inherit line-number))))
 '(helm-swoop-target-line-block-face ((t (:inherit region))))
 '(helm-swoop-target-line-face ((t (:inherit helm-selection))))
 '(helm-swoop-target-word-face ((t (:inherit lazy-highlight))))
 '(helm-visible-mark ((t (:inherit region))))
 '(highlight ((t (:background "#49494e"))))
 '(ido-indicator ((t (:inherit link :underline nil :width condensed))))
 '(ido-only-match ((t (:inherit font-lock-function-name-face))))
 '(ido-subdir ((t (:inherit font-lock-type-face))))
 '(info-header-xref ((t (:inherit info-xref))))
 '(info-node ((t (:inherit link :slant italic :weight bold))))
 '(isearch ((t (:background "hotpink4" :foreground "#b9b9b9"))))
 '(isearch-fail ((t (:inherit isearch))))
 '(ivy-current-match ((t (:inherit highlight))))
 '(ivy-match-required-face ((t (:inherit link :underline nil :weight bold))))
 '(js2-error ((t (:inherit font-lock-warning-face))))
 '(js2-external-variable ((t (:inherit font-lock-constant-face))))
 '(js2-function-call ((t (:inherit default))))
 '(js2-function-param ((t (:inherit font-lock-constant-face))))
 '(js2-instance-member ((t (:inherit font-lock-constant-face))))
 '(js2-jsdoc-value ((t (:inherit js2-jsdoc-tag))))
 '(js2-private-function-call ((t (:inherit default))))
 '(js2-private-member ((t (:inherit default))))
 '(js2-warning ((t (:inherit font-lock-warning-face))))
 '(lazy-highlight ((t (:inherit highlight))))
 '(line-number ((t (:inherit default :foreground "gray29"))))
 '(line-number-current-line ((t (:inherit line-number))))
 '(link ((t (:foreground "#d688d3" :underline t))))
 '(link-visited ((t (:inherit link :foreground "MediumPurple4"))))
 '(linum ((t (:inherit default :foreground "#a0a0a0"))))
 '(magit-blame-heading ((t (:inherit (font-lock-comment-face header-line)))))
 '(magit-branch-current ((t (:inherit magit-branch-local))))
 '(magit-branch-local ((t (:inherit font-lock-variable-name-face :weight bold))))
 '(magit-branch-remote ((t (:inherit font-lock-function-name-face :weight bold))))
 '(magit-cherry-equivalent ((t (:inherit font-lock-string-face))))
 '(magit-diff-added ((t (:inherit (diff-added-bg font-lock-string-face)))))
 '(magit-diff-added-highlight ((t (:inherit magit-diff-added))))
 '(magit-diff-base ((t (:inherit diff-changed))))
 '(magit-diff-base-highlight ((t (:inherit diff-changed))))
 '(magit-diff-context ((t (:inherit font-lock-comment-face :slant normal))))
 '(magit-diff-context-highlight ((t (:inherit magit-diff-context))))
 '(magit-diff-file-heading-highlight ((t nil)))
 '(magit-diff-hunk-heading ((t nil)))
 '(magit-diff-hunk-heading-highlight ((t nil)))
 '(magit-diff-our ((t (:inherit magit-diff-added))))
 '(magit-diff-our-highlight ((t (:inherit magit-diff-added-highlight))))
 '(magit-diff-removed ((t (:inherit (diff-removed-bg font-lock-type-face)))))
 '(magit-diff-removed-highlight ((t (:inherit magit-diff-removed))))
 '(magit-diff-their ((t (:inherit magit-diff-removed))))
 '(magit-diff-their-highlight ((t (:inherit magit-diff-removedt-highlight))))
 '(magit-diffstat-added ((t (:inherit font-lock-string-face))))
 '(magit-diffstat-removed ((t (:inherit font-lock-type-face))))
 '(magit-header-line ((t (:inherit ##))))
 '(magit-log-author ((t (:inherit font-lock-builtin-face))))
 '(magit-log-date ((t nil)))
 '(magit-section-heading ((t (:underline t :weight bold))))
 '(magit-section-highlight ((t nil)))
 '(magit-sequence-head ((t (:inherit font-lock-type-face))))
 '(magit-sequence-part ((t (:inherit font-lock-variable-name-face))))
 '(magit-signature-error ((t (:inherit error :weight normal))))
 '(magit-signature-good ((t (:inherit font-lock-string-face))))
 '(magit-signature-untrusted ((t (:inherit font-lock-constant-face))))
 '(magit-tag ((t (:inherit font-lock-variable-name-face))))
 '(match ((t (:inherit (link highlight) :underline nil :weight bold))))
 '(minibuffer-prompt ((t (:inherit font-lock-keyword-face :weight bold))))
 '(mmm-default-submode-face ((t nil)))
 '(mode-line ((t (:background "#343436" :foreground "#717171" :box (:line-width 4 :color "#343436")))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#363638" :foreground "grey30" :box (:line-width 4 :color "#363638") :weight light))))
 '(org-agenda-done ((t (:foreground "PaleGreen"))))
 '(org-column ((t (:background "grey63" :strike-through nil :underline nil :slant normal :weight normal))))
 '(org-column-title ((t (:background "grey63" :underline t :weight bold))))
 '(org-date ((t (:foreground "gray45" :underline t))))
 '(org-done ((t (:foreground "gray50" :weight bold))))
 '(org-hide ((t (:foreground "#b9b9b9"))))
 '(org-level-1 ((t (:inherit outline-1 :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :weight bold))))
 '(org-level-3 ((t (:inherit font-lock-function-name-face :weight bold))))
 '(org-level-4 ((t (:inherit link :underline nil :weight bold))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "SkyBlue4" :weight bold))))
 '(org-level-6 ((t (:inherit outline-6 :weight bold))))
 '(org-level-7 ((t (:inherit outline-7 :weight bold))))
 '(org-level-8 ((t (:inherit outline-8 :weight bold))))
 '(org-mode-line-clock ((t (:inherit mode-line :foreground "gray60"))))
 '(org-mode-line-clock-overrun ((t (:inherit mode-line :foreground "gray78" :weight bold))))
 '(org-special-keyword ((t (:inherit font-lock-comment-face :foreground "gray50" :slant italic :weight normal))))
 '(org-table ((t (:inherit default))))
 '(org-todo ((t (:foreground "gray40" :weight bold))))
 '(query-replace ((t (:inherit lazy-highlight))))
 '(region ((t (:background "#303032"))))
 '(region-light ((t (:background "grey79"))))
 '(secondary-selection ((t (:inherit nil))))
 '(semantic-highlight-func-current-tag-face ((t (:inherit highlight))))
 '(sh-heredoc ((t (:inherit font-lock-string-face))))
 '(sh-quoted-exec ((t (:inherit font-lock-builtin-face))))
 '(show-paren-match ((t (:background "grey40"))))
 '(show-paren-mismatch ((t (:background "PaleVioletRed4" :foreground "#c0c0c0"))))
 '(smerge-base ((t (:inherit diff-changed))))
 '(smerge-lower ((t (:inherit diff-added-bg))))
 '(smerge-markers ((t (:inherit default))))
 '(smerge-mine ((t (:inherit diff-removed))) t)
 '(smerge-other ((t (:inherit diff-added))) t)
 '(smerge-refined-added ((t (:inherit diff-added))))
 '(smerge-refined-removed ((t (:inherit diff-removed))))
 '(smerge-upper ((t (:inherit diff-removed-bg))))
 '(speedbar-tag-face ((t (:inherit link :underline nil))))
 '(swiper-line-face ((t (:inherit highlight))))
 '(swiper-match-face-2 ((t (:inherit isearch-lazy-highlight-face))))
 '(swiper-match-face-3 ((t (:inherit isearch-lazy-highlight-faec))))
 '(swiper-match-face-4 ((t (:inherit isearch-lazy-highlight-face))))
 '(trailing-whitespace ((t (:background "orchid1"))))
 '(vertical-border ((t (:inherit border))))
 '(wgrep-face ((t (:inherit diff-added-bg))))
 '(wgrep-reject-face ((t (:inherit font-lock-warning-face))))
 '(widget-field ((t (:background "grey30"))))
 '(word-count-marker-face ((t nil)) t))



