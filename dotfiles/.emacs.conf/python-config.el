;;;; Python specific confuguration settings

;; (require 'python)

;; Configure Python configuration to use ipython
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
        elpy-module-flymake
        elpy-module-highlight-indentation
        elpy-module-pyvenv
        elpy-module-yasnippet
        elpy-module-django))

(elpy-enable)

(define-key elpy-mode-map (kbd "C-x C-e") 'elpy-shell-send-statement)
(define-key elpy-mode-map (kbd "C-c C-r") 'elpy-shell-send-region-or-buffer)
(define-key elpy-mode-map (kbd "C-c C-t") 'elpy-shell-send-top-statement)
(define-key elpy-mode-map (kbd "C-c C-p") 'elpy-shell-switch-to-shell)

;; (require 'conda)
;; if you want interactive shell support, include:
;; (conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
;; (conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
;; (conda-env-autoactivate-mode t)

;; (custom-set-variables
;;  '(conda-anaconda-home "~/anaconda3"))
