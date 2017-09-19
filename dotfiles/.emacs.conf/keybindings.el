
;;;; Global Key Bindings

(defvar super-keys-minor-mode-map
  (make-sparse-keymap)
  "Super keys: these keys should always work anywhere in emacs.")

;; These basic keybindings should always work.
(define-key super-keys-minor-mode-map (kbd "C-r") 'backward-char)
(define-key super-keys-minor-mode-map (kbd "M-r") 'backward-word)
(define-key super-keys-minor-mode-map (kbd "C-b") nil)
(define-key super-keys-minor-mode-map (kbd "M-b") nil)
(define-key super-keys-minor-mode-map (kbd "C-M-r") 'sp-backward-sexp)

(define-minor-mode super-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter "SK"
  :keymap super-keys-minor-mode-map)

(add-hook 'term-mode-hook (lambda () (super-keys-minor-mode -1)))

(super-keys-minor-mode t)

;; Global keymaps can be done using `global-set-key', which takes
;; "\key" and 'command as arguments. Mode specific keymaps are bound
;; using the `define-key' function, with the specific keymap, "\key"
;; and 'command as arguments.
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-M-;") 'comment-region)
(global-set-key (kbd "C-M-:") 'uncomment-region)
(global-set-key (kbd "C-x C-r") 'rename-buffer)

;; Ido
(when (require 'ido nil 'noerror)
  (define-key ido-common-completion-map (kbd "C-r") 'ido-magic-backward-char)
  (define-key ido-common-completion-map (kbd "C-M-s") 'ido-prev-match)
  ;; Prevent space completion, useless, we already have tab
  (define-key ido-common-completion-map " " nil))

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x g") 'ctl-x-5-prefix)

;; General movement
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

(global-set-key (kbd "C-c g") 'magit-status)

;; Org-mode shortcuts
(when (require 'org nil 'noerror)
 (define-key org-mode-map (kbd "C-M-n") 'outline-next-visible-heading)
 (define-key org-mode-map (kbd "C-M-p") 'outline-previous-visible-heading)

 (define-key org-mode-map (kbd "C-c l") 'org-store-link)
 (define-key org-mode-map (kbd "C-c a") 'org-agenda))

;; Change mac command keys
(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;;;; Helm

(when (require 'helm nil 'noerror)
  (global-set-key (kbd "M-m") 'smex)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-s") 'helm-swoop)
  (global-set-key (kbd "C-M-s") 'helm-swoop-back-to-last-point)

  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
  (define-key helm-map (kbd "C-r") 'backward-char)
  (define-key helm-map (kbd "M-r") 'backward-word)

  ;; The default helm prefix "C-x c" is quite close to "C-x C-c",
  ;; which quits Emacs. Change helm command prefix to "M-h".
  (global-unset-key (kbd "C-x c"))
  (if (boundp 'no-helm-rebind-prefix)
      ;; For some reason this does not work with `git-config.el`, so
      ;; use a regular prefix "C-c h". Note: We must set "C-c h"
      ;; globally, because we cannot change `helm-command-prefix-key'
      ;; once `helm-config' is loaded.
      (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-set-key (kbd "M-h") 'helm-command-prefix)
    (global-set-key (kbd "M-h M-i") 'helm-semantic-or-imenu)
    (global-set-key (kbd "M-h i") 'helm-semantic-or-imenu)))

;;;; ISpell

(global-set-key (kbd "M-s s") 'ispell)
(global-set-key (kbd "M-s w") 'ispell-word)

;; Disable Emacs Web Browser keybinding
(global-unset-key (kbd "M-s M-w"))
