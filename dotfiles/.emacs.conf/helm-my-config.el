;;;; Helm specific configuration settings

;; Load Helm
(require 'helm)
(require 'helm-config)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

;; open helm buffer inside current window, not occupy whole other window
(setq helm-split-window-in-side-p t)

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

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)

;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; Don't use pre-input for helm-swoop
(setq helm-swoop-pre-input-function (lambda () ""))
