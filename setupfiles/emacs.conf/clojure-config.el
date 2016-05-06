;;;; Clojure Environment Configuration

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

;; extra syntax hilighting for clojurec and clojurescript modes.
(font-lock-add-keywords
 'clojurec-mode
 `((,(concat "(\\(?:\.*/\\)?"
             (regexp-opt clojure-built-in-vars t)
             "\\>")
    1 font-lock-builtin-face)))

(font-lock-add-keywords
 'clojurec-mode
 `((,(concat "\\<"
             (regexp-opt clojure-built-in-dynamic-vars t)
             "\\>")
    0 font-lock-builtin-face)))

(font-lock-add-keywords
 'clojurescript-mode
 `((,(concat "(\\(?:\.*/\\)?"
             (regexp-opt clojure-built-in-vars t)
             "\\>")
    1 font-lock-builtin-face)))

(font-lock-add-keywords
 'clojurescript-mode
 `((,(concat "\\<"
             (regexp-opt clojure-built-in-dynamic-vars t)
             "\\>")
    0 font-lock-builtin-face)))

;; Use clojure mode for other extensions
;; (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))
;; (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))


;;;; Clojurescript indenting

(load "clojurescript-indenting.el")


;;;; Cider

;; disable cider's dynamic font-lock overrides
(setq cider-font-lock-dynamically nil)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-repl-mode-map (kbd "M-r") 'backward-word)))

