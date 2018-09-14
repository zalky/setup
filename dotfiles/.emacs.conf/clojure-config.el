;;;; Clojure Configuration

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; But don't show unused reader conditionals. This is done based on
;; repl connection, and you are often not connected with the same repl
;; as the code you are working on. And for some reason it defaults to
;; cljs.
(setq cider-font-lock-reader-conditionals nil)

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

(defvar my-clojure-keywords
  '("with-let"
    "with-entity"
    "with-uuids"))

(font-lock-add-keywords
 'clojure-mode
 `((,(concat "(\\(?:\.*/\\)?"
             (regexp-opt my-clojure-keywords t)
             "\\>")
    1 font-lock-keyword-face)))

(font-lock-add-keywords
 'clojurec-mode
 `((,(concat "(\\(?:\.*/\\)?"
             (regexp-opt my-clojure-keywords t)
             "\\>")
    1 font-lock-keyword-face)))

(font-lock-add-keywords
 'clojurescript-mode
 `((,(concat "(\\(?:\.*/\\)?"
             (regexp-opt my-clojure-keywords t)
             "\\>")
    1 font-lock-keyword-face)))

;; No prefix notation for clojure refactor
(setq cljr-favor-prefix-notation nil)

;; have nREPL start browser-connected figwheel repl.
(setq cider-cljs-lein-repl
      "(do (user/run)
           (user/browser-repl))")

;;;; Cider

;; use pprint by default to print to repl
(setq cider-repl-use-pretty-printing t)

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

;; Disable cider documentation tooltips.
(setq cider-use-tooltips nil)

;; Disable cider fringe loaded code indicators.
(setq cider-use-fringe-indicators nil)

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
     (define-key cider-repl-mode-map (kbd "C-c C-r") 'clojure-refactor-map)))


;; Indenting

(load "clojure-indenting.el")

