;;;; Helm specific configuration settings

;; Load Helm
(require 'helm)
(require 'helm-config)

(helm-mode t)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;; move to end or beginning of source when reaching top or bottom of source.
(setq helm-move-to-line-cycle-in-source t)

;; search for library in `require' and `declare-function' sexp.
(setq helm-ff-search-library-in-sexp t)

;; scroll 8 lines other window using M-<next>/M-<prior>
(setq helm-scroll-amount 8)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color t)

(setq helm-ff-file-name-history-use-recentf t)
(setq helm-buffer-max-length 30)

;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; Don't use pre-input for helm-swoop
(setq helm-swoop-pre-input-function (lambda () ""))

(setq helm-truncate-lines t)

;; In helm, do not use 
(dolist (n (number-sequence 1 9))
  (define-key helm-map (kbd (format "M-%s" n))
    `(lambda ()
       (interactive)
       (helm-select-nth-action ,(1- n)))))

(setq helm-display-header-line nil)

(setq helm-prevent-escaping-from-minibuffer nil)

(setq helm-split-window-default-side 'same)
(setq helm-split-window-inside-p nil)
;; (setq helm-swoop-split-window-function 'helm-default-display-buffer)

;; Helm window splitting
(setq helm-swoop-split-direction 'split-window-vertically)
(setq helm-swoop-split-with-multiple-windows t)

