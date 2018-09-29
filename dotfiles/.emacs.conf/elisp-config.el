;;;; Elisp Configuration

;; Common Lisp extensions
(require 'cl-lib)
 
;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; IELM Keybindings
(eval-after-load 'ielm
  '(progn
     (define-key ielm-map (kbd "M-r") 'backward-word)
     (define-key ielm-map (kbd "C-j") 'electric-newline-and-maybe-indent)))
