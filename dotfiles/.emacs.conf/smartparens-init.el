;;;;;;;;;
;; global
(require 'smartparens-config)
(smartparens-global-mode 1)

;; Don't highlight space inside newly created s-exps.
(setq sp-highlight-pair-overlay nil)

;; Use smartparens in the minibuffer. Useful for `M-:`.
(setq sp-ignore-modes-list
      (delete 'minibuffer-inactive-mode sp-ignore-modes-list))

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") nil)
(define-key smartparens-mode-map (kbd "C-M-r") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-e") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-S-d") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-S-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
(define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-(") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-(") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-)") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "M-s M-s") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "M-s M-S") 'sp-split-sexp)
(define-key smartparens-mode-map (kbd "M-s M-c") 'sp-convolute-sexp)
(define-key smartparens-mode-map (kbd "M-s M-a") 'sp-absorb-sexp)
(define-key smartparens-mode-map (kbd "M-s M-e") 'sp-emit-sexp)
(define-key smartparens-mode-map (kbd "M-s M-n") 'sp-add-to-next-sexp)
(define-key smartparens-mode-map (kbd "M-s M-p") 'sp-add-to-previous-sexp)
(define-key smartparens-mode-map (kbd "M-s M-j") 'sp-join-sexp)
(define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-S-<backspace>") 'sp-backward-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
(define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :wrap "C-*" :skip-match 'sp--gfm-skip-asterisk)
  (sp-local-pair "_" "_" :wrap "C-_")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

(defun sp--gfm-skip-asterisk (ms mb me)
  (save-excursion
    (goto-char mb)
    (save-match-data (looking-at "^\\* "))))

;;; org-mode
(sp-with-modes 'org-mode
  (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
  (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
  (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "«" "»"))

(defun sp--org-skip-asterisk (ms mb me)
  (or (and (= (line-beginning-position) mb)
           (eq 32 (char-after (1+ mb))))
      (and (= (1+ (line-beginning-position)) me)
           (eq 32 (char-after me)))))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil
                 :wrap "C-("
                 :pre-handlers '(my-add-space-before-sexp-insertion)
                 :post-handlers '(my-add-space-after-sexp-insertion)))



(defun my-add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (sp-get-pair id :cl-l))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))

(defun my-add-space-before-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char (length id))
      (when (or (eq (char-syntax (preceding-char)) ?w)
                (and (looking-back (sp--get-closing-regexp))
                     (not (eq (char-syntax (preceding-char)) ?'))))
        (insert " ")))))

;;; C++
(sp-with-modes '(malabar-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
(sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                    ("* ||\n[i]" "RET")))

;;; haskell mode
(sp-with-modes '(haskell-mode)
  (sp-local-pair "'" nil :unless '(my-after-symbol-p))
  (sp-local-pair "\\(" nil :actions nil))

(defun my-after-symbol-p (_id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char 1)
      (looking-back "\\sw\\|\\s_\\|\\s'"))))

;;; Clojure modes
(sp-with-modes '(clojure-mode clojurec-mode clojurescript-mode)
  (sp-local-pair "'" nil)
  (sp-local-pair "'" nil))

(sp-pair "'" nil :actions :rem)
(sp-pair "`" nil :actions :rem)

;;; Other modes

(sp-with-modes '(php-mode)
  (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                             (my-php-handle-docstring "RET")))
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

(defun my-php-handle-docstring (&rest _ignored)
  (-when-let (line (save-excursion
                     (forward-line)
                     (thing-at-point 'line)))
    (cond
     ((string-match-p "function" line)
      (save-excursion
        (insert "\n")
        (let ((args (save-excursion
                      (forward-line)
                      (my-php-get-function-args))))
          (--each args
            (insert (format "* @param %s\n" it)))))
      (insert "* "))
     ((string-match-p ".*class\\|interface" line)
      (save-excursion (insert "\n*\n* @author\n"))
      (insert "* ")))
    (let ((o (sp--get-active-overlay)))
      (indent-region (overlay-start o) (overlay-end o)))))
