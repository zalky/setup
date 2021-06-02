;;;; Python specific confuguration settings

;; Elpy configuration

;; The latest elpy package has a bug where it sends ^G control signals
;; to a running IPython process, making python REPL workflow
;; unusable. We manually revert to a previous version of elpy that is
;; known to work.
(add-to-list 'load-path "~/local/share/elisp/elpy-1.25.0")

(require 'elpy)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; While good, can't easily do plotting in-line in emacs.
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)

;; (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")

(setq elpy-modules
      '(elpy-module-sane-defaults
        elpy-module-eldoc
        elpy-module-highlight-indentation
        elpy-module-yasnippet
        elpy-module-django
        ;; elpy-module-pyvenv
        ;; elpy-module-flymake
        ))

(elpy-enable)

(define-key elpy-mode-map (kbd "C-x C-e") 'elpy-shell-send-statement)
(define-key elpy-mode-map (kbd "C-c C-r") 'elpy-shell-send-region-or-buffer)
(define-key elpy-mode-map (kbd "C-c C-t") 'elpy-shell-send-top-statement)
(define-key elpy-mode-map (kbd "C-c C-p") 'elpy-shell-switch-to-shell)


;; Conda configuration

;; conda.el configuration begins
(require 'conda)
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
(conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
(conda-env-autoactivate-mode t)
;; from https://stackoverflow.com/a/56722815
(setq conda-anaconda-home (expand-file-name "~/opt/anaconda3"))
(setq conda-env-home-directory (expand-file-name "~/opt/anaconda3"))

(add-to-list 'exec-path (expand-file-name "~/opt/anaconda3/bin"))
(setenv "PATH" "~/opt/anaconda3/bin:$PATH" '("PATH"))

;; We must also ensure that the shell PATH in .bashrc and .profile are
;; properly set.
