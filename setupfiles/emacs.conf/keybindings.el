;;;; Global Key Bindings

;; Global keymaps can be done using `global-set-key', which takes
;; "\key" and 'command as arguments. Mode specific keymaps are bound
;; using the `define-key' function, with the specific keymap, "\key"
;; and 'command as arguments.
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-M-;") 'comment-region)
(global-set-key (kbd "C-M-:") 'uncomment-region)

;; Basic navigation
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

;; Ido
(define-key ido-common-completion-map (kbd "C-r") 'ido-magic-backward-char)
(define-key ido-common-completion-map (kbd "C-M-s") 'ido-prev-match)

;; ;; Interactive search key bindings. By default, C-s runs
;; ;; isearch-forward, so this swaps the bindings.
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)

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

;;;; Helm

(global-set-key (kbd "M-m") 'smex)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-s") 'helm-swoop)
(global-set-key (kbd "C-M-s") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)

(define-key helm-map (kbd "C-M-n") 'helm-next-source)
(define-key helm-map (kbd "C-M-p") 'helm-previous-source)
(define-key helm-map (kbd "C-r") 'backward-char)
(define-key helm-map (kbd "M-r") 'backward-word)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)
