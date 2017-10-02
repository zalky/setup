;;;; Configure packages, tools, dependencies and repositories

(require 'package)

;; Define package repositories
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(setq package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu"          . 5)
        ("melp"         . 0)))

;; The packages you want installed. You can also install these
;; manually with M-x package-install Add in your own as you wish:
(defvar my-packages
  '(;; load smartparens mode instead of paredit: paredit is useless as
    ;; there is no way to edit keybindings
    smartparens
    
    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; Provides refactoring support for clojure.
    clj-refactor

    ;; ;; allow ido usage in as many contexts as possible. see
    ;; ;; workflow.el for a description of ido
    ;; ido-ubiquitous

    ;; ;; Enhances M-x to allow easier execution of commands. Provides a
    ;; ;; filterable list of possible commands in the minibuffer
    ;; ;; http://www.emacswiki.org/emacs/Smex
    ;; smex

    ;; Emacs incremental completion and selection narrowing framework
    ;; https://emacs-helm.github.io/helm/
    helm

    ;; Search hopping powered by helm
    helm-swoop

    ;; Project navigation
    helm-projectile

    ;; Node.js integration
    indium

    ;; Vue integration
    vue-mode

    ;; Javascript mode
    js2-mode

    ;; edit html tags like sexps
    tagedit

    ;; highlight numbers
    highlight-numbers

    ;; git integration
    magit))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((indium . "melpa"))))

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.  This
;; also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Actually install packages that have not been installed yet.
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Load word-count-mode
(require 'word-count)

;; Semantic is a package that provides language-aware editing commands
;; based on source code parsers. Semantic can do things such as prompt
;; for the name of a function defined in the current file, and move
;; point there, or Display a list of possible completions for the
;; symbol at point.
(require 'semantic/sb)

;; Prevent semantic from littering directories with cache files
(setq semanticdb-default-save-directory "~/.emacs.d/semanticdb/")

;; Conifgure Emacs Code Browser
;; (load "ecb-config.el")

;; Load documentation system
(require 'info)

;; The advice feature lets you add to the existing definition of a
;; function, by advising the function. This is a cleaner method than
;; redefining the whole function. But be careful as this can
;; break. Better to use hooks to modify functions that expect user
;; customization.
(require 'advice)

;;;; Flymd

(defvar flymd-location
  "~/src/elisp/flymd")

(when (file-exists-p (concat flymd-location "/flymd.el"))
  (add-to-list 'load-path flymd-location)

  (require 'flymd)

  (defun my-flymd-browser-function (url)
    (let ((process-environment (browse-url-process-environment)))
      (apply 'start-process
             (concat "firefox " url)
             nil
             "/usr/bin/open"
             (list "-a" "firefox" url))))

  (setq flymd-browser-open-function 'my-flymd-browser-function))

