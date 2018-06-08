;;;; Configuration settings for: Display

;; Enable font lock mode for all major modes that support font-lock
(global-font-lock-mode t)

;; Define custom face attributes
;; (setq my-font "Fira Mono") ;; On Windows: "Consolas"
(setq my-fgcolor "#303030");; "#b9b9b9" ;; "#cccccc"
(setq my-bgcolor "#c0c0c0") ;; "#303030") ;; "#303030

;; Set default frame attributes and initial position
(setq my-frame-alist `((width . 90)
                       (height . 46)
                       (ns-transparent-titlebar . t)
                       (frame-title-format . "")))

;; You can specify geometry-related options for the initial frame,
;; however they won't take effect until Emacs reads `.emacs', which
;; happens after first creating the frame. Therefore, set the selected
;; frame's position instead.
(setq default-frame-alist (append my-frame-alist default-frame-alist))
(setq initial-frame-alist (append my-frame-alist initial-frame-alist))

(set-frame-position (selected-frame) 400 30)

;; Set face attributes for existing faces (t: default for all frames)

(set-face-attribute 'mode-line-highlight nil)

(set-face-attribute 'minibuffer-prompt t
                    :weight 'bold
                    :foreground "gray30")

(setq ansi-term-color-vector
      [default
        default
        font-lock-type-face
        font-lock-variable-name-face
        font-lock-comment-face
        font-lock-function-name-face
        font-lock-string-face
        font-lock-constant-face
        font-lock-builtin-face])

(defface diff-added-bg
  '((t :background "#aebbb1"))
  "Face for added comparative regions."
  :group 'diff-mode)

(defface diff-removed-bg
  '((t :background "#c4b5ba"))
  "Face for removed comparative regions."
  :group 'diff-mode)

(defface diff-added-fine-bg
  '((t :background "#a9d0b2"))
  "Face for fine added comparative regions."
  :group 'diff-mode)

(defface diff-removed-fine-bg
  '((t :background "#debfcd"))
  "Face for fine removed comparative regions."
  :group 'diff-mode)

(defface diff-changed-bg
  '((t :background "#b5b3c7"))
  "Face for changed comparative regions."
  :group 'diff-mode)

(defface diff-changed-fine-bg
  '((t :background "#b2bde8"))
  "Face for fine changed comparative regions."
  :group 'diff-mode)

