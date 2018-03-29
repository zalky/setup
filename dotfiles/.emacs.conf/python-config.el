;;;; Python specific confuguration settings

(require 'python)

;; Configure Python configuration to use ipython
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "-i --simple-prompt")

;; If desired, start ipython in pylab mode. But this auto-imports a
;; bunch of names into global namespace, and convenience is not worth
;; the tradeoff in namespace confusion.
;; (setq python-shell-interpreter-args "--pylab")

;; Keybindings
(define-key inferior-python-mode-map (kbd "M-r") 'backward-word)
(define-key inferior-python-mode-map (kbd "M-h") 'comint-history-isearch-backward-regexp)

(require 'conda)
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
(conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
(conda-env-autoactivate-mode t)

(custom-set-variables
 '(conda-anaconda-home "~/anaconda3"))
