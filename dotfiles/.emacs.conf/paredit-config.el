
;; Require smartparens just for `sp-unwrap-sexp`
(require 'smartparens)
(require 'cider)
(require 'paredit)

(define-key paredit-mode-map (kbd "M-;") 'comment-dwim)
(define-key paredit-mode-map (kbd "C-)") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-(") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-(") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-)") 'paredit-backward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-<backspace>") 'sp-unwrap-sexp)
(define-key paredit-mode-map (kbd "C-M-S-<backspace>") 'sp-backward-unwrap-sexp)
(define-key paredit-mode-map (kbd "C-M-i") 'paredit-forward-down)
(define-key paredit-mode-map (kbd "C-M-u") 'paredit-backward-up)
(define-key paredit-mode-map (kbd "C-M-o") 'paredit-backward-down)
(define-key paredit-mode-map (kbd "C-M-p") 'paredit-forward-up)

;; This is a workaround for force curly brace matching in all contexts
;; where paredit mode is active
(define-key paredit-mode-map (kbd "{") 'paredit-open-curly)
(define-key paredit-mode-map (kbd "}") 'paredit-close-curly)

(defvar lisp-modes
  '(lisp-mode
    emacs-lisp-mode
    clojure-mode
    clojurec-mode
    clojurescript-mode
    cider-repl-mode)
  "Lisp modes where we want paredit to behave differently than
  regular progn modes.")

(defun progn-paredit-space-for-delimiter-p (endp delim)
  "Prevent paredit from inserting a space, except in lisp modes."
  (member major-mode lisp-modes))

(add-hook 'paredit-space-for-delimiter-predicates
          #'progn-paredit-space-for-delimiter-p)

(add-hook 'prog-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'inferior-python-mode-hook #'paredit-mode)
