
;; Require smartparens just for `sp-unwrap-sexp`
(require 'smartparens)
(require 'cider)
(require 'paredit)

(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'prog-mode-hook #'paredit-mode)

(define-key paredit-mode-map (kbd "M-;") 'comment-dwim)
(define-key paredit-mode-map (kbd "C-)") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-(") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-(") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-)") 'paredit-backward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-<backspace>") 'sp-unwrap-sexp)
(define-key paredit-mode-map (kbd "C-M-S-<backspace>") 'sp-unwrap-sexp)
(define-key paredit-mode-map (kbd "C-M-i") 'paredit-forward-down)
(define-key paredit-mode-map (kbd "C-M-u") 'paredit-backward-up)
(define-key paredit-mode-map (kbd "C-M-o") 'paredit-backward-down)
(define-key paredit-mode-map (kbd "C-M-p") 'paredit-forward-up)


