
;;;; Global Key Bindings

(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)

(defun uber-save ()
  (interactive)
  (call-interactively
   (if (eq major-mode 'Custom-mode)
       'Custom-save
     'save-buffer)))

;; Global keymaps can be done using `global-set-key', which takes
;; "\key" and 'command as arguments. Mode specific keymaps are bound
;; using the `define-key' function, with the specific keymap, "\key"
;; and 'command as arguments.
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "R") 'rename-buffer)
(global-set-key (kbd "C-M-q") 'unfill-paragraph)
(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x s") 'uber-save)
(global-set-key (kbd "C-x DEL") 'kill-this-buffer)
(global-set-key (kbd "C-x e") 'eval-last-sexp)
(global-set-key (kbd "C-x q") 'query-replace)

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-j") 'hippie-expand)


;;;; General movement

(defun next-line-3 ()
  (interactive)
  (setq this-command 'next-line)
  (next-line 3))

(defun previous-line-3 ()
  (interactive)
  (setq this-command 'previous-line)
  (previous-line 3))


;;;; Boon Modal Editing

(require 'boon-core)
(require 'boon-main)

;; TODO:
;; Commenting

(defvar boon-h-map nil
  "Makes h prefix keys available in command mode.")

(define-prefix-command 'boon-h-map)
(set-keymap-parent boon-h-map help-map)

;; Override boon-mode minor mode to enable in minibuffer
(define-globalized-minor-mode boon-mode boon-local-mode
  (lambda () (boon-local-mode 1))
  :group 'boon)

;; Supress messages when switching modes in minibuffer, start in insert mode
(defun boon-minibuf-hook ()
  (setq-local inhibit-message t)
  (turn-off-boon-mode))

;; Turn on boon-mode globally on startup to prevent weirdness from
;; previously saved desktop state.
(boon-mode 1)

(defun boon-toggle-state ()
  (interactive)
  (if boon-command-state
      (boon-set-insert-like-state)
    (boon-set-command-state)))

(global-set-key (kbd "\\") 'boon-mode)
(global-set-key (kbd "M-\\") 'boon-local-mode)
(define-key minibuffer-local-map (kbd "\\") 'boon-local-mode)

(define-key boon-command-map (kbd "x") 'boon-x-map)
(define-key boon-command-map (kbd "c") 'boon-c-god)
(define-key boon-command-map (kbd "h") 'boon-h-map)
(define-key boon-command-map (kbd "p") 'projectile-command-map)

(global-set-key (kbd "M-l") 'forward-char)
(global-set-key (kbd "M-j") 'backward-char)
(global-set-key (kbd "M-i") 'previous-line)
(global-set-key (kbd "M-k") 'next-line)
(define-key boon-moves-map (kbd "i") 'previous-line-3)
(define-key boon-moves-map (kbd "k") 'next-line-3)
(define-key boon-moves-map (kbd "l") 'forward-word)
(define-key boon-moves-map (kbd "j") 'backward-word)
(define-key boon-moves-map (kbd "[") 'backward-paragraph)
(define-key boon-moves-map (kbd "]") 'forward-paragraph)
(define-key boon-moves-map (kbd ">") 'end-of-buffer)
(define-key boon-moves-map (kbd "<") 'beginning-of-buffer)
(define-key boon-moves-map (kbd ";") 'recenter-top-bottom)
(define-key boon-moves-map (kbd "v") 'scroll-up-command)
(define-key boon-moves-map (kbd "C-v") 'scroll-down-command)

(define-key boon-command-map (kbd "u") 'sp-backward-sexp)
(define-key boon-command-map (kbd "o") 'sp-forward-sexp)
(define-key boon-command-map (kbd "M-u") 'sp-backward-up-sexp)
(define-key boon-command-map (kbd "M-o") 'sp-up-sexp)

(define-key boon-command-map (kbd "0") 'sp-forward-slurp-sexp)
(define-key boon-command-map (kbd "9") 'sp-forward-barf-sexp)
(define-key boon-command-map (kbd "M-0") 'sp-backward-barf-sexp)
(define-key boon-command-map (kbd "M-9") 'sp-backward-slurp-sexp)
(define-key boon-command-map (kbd "f") 'kill-sexp)
(define-key boon-command-map (kbd "r") 'backward-kill-sexp)
(define-key boon-command-map (kbd "-") 'sp-unwrap-sexp)
(define-key boon-command-map (kbd "M--") 'sp-backward-unwrap-sexp)

(define-key boon-command-map (kbd "C-c C-q") 'quoted-insert)
(define-key boon-command-map (kbd "RET") 'newline)
(define-key boon-command-map (kbd "DEL") 'sp-backward-delete-char)
(define-key boon-command-map (kbd "d") 'delete-forward-char)
(define-key boon-command-map (kbd "M-DEL") 'backward-kill-word)
(define-key boon-command-map (kbd "M-d") 'kill-word)
(define-key boon-command-map (kbd "M-w") 'kill-ring-save)
(define-key boon-command-map (kbd "w") 'kill-region)
(define-key boon-command-map (kbd "y") 'yank)
(define-key boon-command-map (kbd "SPC") 'set-mark-command)
(define-key boon-command-map (kbd "M-SPC") 'mark-sexp)
(define-key boon-command-map (kbd "t") 'tab-to-tab-stop)
(define-key boon-command-map (kbd "M-\\") 'delete-horizontal-space)
(define-key boon-command-map (kbd "'") 'indent-region)

;; (defun my-self-insert-command ()
;;   "Workaround since you can't bind self-insert-command directly."
;;   (interactive)
;;   (self-insert-command 1))

;; (define-key boon-command-map (kbd "(") 'my-self-insert-command)
;; (define-key boon-command-map (kbd ")") 'my-self-insert-command)
;; (define-key boon-command-map (kbd "[") 'my-self-insert-command)
;; (define-key boon-command-map (kbd "]") 'my-self-insert-command)
;; (define-key boon-command-map (kbd "{") 'my-self-insert-command)
;; (define-key boon-command-map (kbd "}") 'my-self-insert-command)
;; (define-key boon-command-map (kbd "\"") 'my-self-insert-command)

(define-key boon-command-map (kbd "e") 'move-end-of-line)
(define-key boon-command-map (kbd "a") 'move-beginning-of-line)
(define-key boon-command-map (kbd "z") 'undo)

;; Window
(global-set-key (kbd "C-x j") 'windmove-left)
(global-set-key (kbd "C-x l") 'windmove-right)
(global-set-key (kbd "C-x i") 'windmove-up)
(global-set-key (kbd "C-x k") 'windmove-down)
(global-set-key (kbd "C-x =") 'balance-windows)
(global-set-key (kbd "C-x 9") 'transpose-buffers)
(global-set-key (kbd "S-C-M-j") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-M-l") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-M-k") 'shrink-window)
(global-set-key (kbd "S-C-M-i") 'enlarge-window)
(global-set-key (kbd "C-.") 'scroll-left)
(global-set-key (kbd "C-,") 'scroll-right)

;; Major Modes
(define-key boon-command-map (kbd "b") 'projectile-switch-to-buffer)
(define-key boon-command-map (kbd "!") 'shell-command)
(define-key boon-command-map (kbd "E") 'eval-expression)
(global-set-key (kbd "C-c C-g") 'magit-status)
(global-set-key (kbd "C-c C-b") 'magit-blame)
(global-set-key (kbd "C-c l") 'org-store-link)


;;;; Secial command mapcat

;; avoid single character commands in special mode, they mess with
;; special major modes like dired and magit
(define-key boon-special-map (kbd "x") 'boon-x-map)
(define-key boon-special-map (kbd "M-l") 'forward-char)
(define-key boon-special-map (kbd "M-j") 'backward-char)
(define-key boon-special-map (kbd "M-i") 'previous-line)
(define-key boon-special-map (kbd "M-k") 'next-line)
(define-key boon-special-map (kbd ";") 'recenter-top-bottom)


;;;; Term

;; Start term mode in command mode, and make boon toggle key available.
(add-hook 'term-load-hook
          (lambda ()
            (term-line-mode)
            (define-key term-raw-map (kbd "\\") 'boon-mode)))

(defun boon-term-sync-input-mode ()
  "Sync char and line term input modes with boon on off state."
  (if (derived-mode-p 'term-mode)
      (if (bound-and-true-p boon-local-mode)
          (term-line-mode)
        (term-char-mode))))

(add-hook 'boon-local-mode-hook 'boon-term-sync-input-mode)


;;;; Helm

(when (require 'helm nil 'noerror)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-b") 'helm-mini)
  (global-set-key (kbd "B") 'helm-mini)
  (global-set-key (kbd "C-x f") 'helm-projectile-find-file)
  (global-set-key (kbd "C-x p") 'helm-projectile-switch-project)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x F") 'helm-find-files)

  (define-key helm-map (kbd "M-j") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-M-n") 'helm-next-source)
  (define-key helm-map (kbd "C-M-p") 'helm-previous-source)
  (define-key helm-map (kbd "C-r") 'backward-char)
  (define-key helm-map (kbd "M-r") 'backward-word)
  
  (define-key helm-map (kbd "M-k") 'helm-next-line)
  (define-key helm-map (kbd "M-i") 'helm-previous-line)

  ;; Without this, we cannot access helm-M-x in terminals.
  (add-hook 'term-mode-hook
            (lambda ()
              (define-key term-raw-map (kbd "M-x") 'helm-M-x)))

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


;; Helm Swoop

(when (require 'helm-swoop nil 'noerror)
  (global-set-key (kbd "C-M-s") 'helm-swoop-back-to-last-point)
  (define-key helm-swoop-map (kbd "M-k") 'helm-swoop-next-line)
  (define-key helm-swoop-map (kbd "M-i") 'helm-swoop-previous-line)
  (define-key boon-command-map (kbd "s") 'helm-swoop))

;;;; ISpell

(global-set-key (kbd "M-s s") 'ispell)
(global-set-key (kbd "M-s w") 'ispell-word)

;; Disable Emacs Web Browser keybinding
(global-unset-key (kbd "M-s M-w"))
