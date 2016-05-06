;;;; Global Key Bindings

;; Global keymaps can be done using `global-set-key', which takes
;; "\key" and 'command as arguments. Mode specific keymaps are bound
;; using the `define-key' function, with the specific keymap, "\key"
;; and 'command as arguments.
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-M-;") 'comment-region)
(global-set-key (kbd "C-M-:") 'uncomment-region)

;; Basic navigation
;; Use minor mode to ensure these basic navigation keys are always
;; available.
(defvar basic-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (global-set-key (kbd "C-r") 'backward-char)
    (global-set-key (kbd "M-r") 'backward-word)
    (global-set-key (kbd "C-b") nil)
    (global-set-key (kbd "M-b") nil)
    (global-set-key (kbd "C-M-r") 'sp-backward-sexp)
    map)
  "basic-keymap-minor-mode keymap.")

(define-minor-mode basic-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " basic-keys")

(add-hook 'after-load-functions 'my-keys-have-priority)

(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other
  minor modes. Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'basic-keys-minor-mode)
    (let ((mykeys (assq 'basic-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'basic-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist mykeys))))

(basic-keys-minor-mode t)

;; Ivy
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "M-m") 'counsel-imenu)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(define-key ivy-minibuffer-map (kbd "C-r") nil)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings to regexp.
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)
;; (define-key isearch-mode-map (kbd "C-r") nil)
;; (define-key isearch-mode-map (kbd "M-r") nil)
;; (define-key isearch-mode-map (kbd "C-M-s") 'isearch-repeat-backward)

;; Ido
;; (define-key ido-common-completion-map (kbd "C-r") 'ido-magic-backward-char)
;; (define-key ido-common-completion-map (kbd "C-M-s") 'ido-prev-match)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x g") 'ctl-x-5-prefix)

;; Org-mode shortcuts
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(global-set-key (kbd "S-C-M-j") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-M-l") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-M-k") 'shrink-window)
(global-set-key (kbd "S-C-M-i") 'enlarge-window)

(global-set-key (kbd "C-x 9") 'transpose-buffers)
(global-set-key (kbd "C-c l") 'org-store-link)

(global-set-key (kbd "S-C-j") 'windmove-left)
(global-set-key (kbd "S-C-i") 'windmove-up)
(global-set-key (kbd "S-C-k") 'windmove-down)
(global-set-key (kbd "S-C-l") 'windmove-right)

(global-set-key (kbd "M-t") 'tab-to-tab-stop)

;; Change mac command keys
(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

;; Key binding to use "hippie expand" for text autocompletion
(global-set-key (kbd "M-/") 'hippie-expand)

;; Toggle word-wrap
(global-set-key [f12] 'toggle-truncate-lines)

;; Comments
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;;;; Smartparens
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") nil)
(define-key smartparens-mode-map (kbd "C-M-r") 'sp-backward-sexp)

(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

(define-key smartparens-mode-map (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-e") 'sp-end-of-sexp)

;; (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
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
