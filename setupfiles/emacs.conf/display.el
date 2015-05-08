;;;; Configuration settings for: Display

;; Enable font lock mode for all major modes that support font-lock
(global-font-lock-mode t)

;; Define custom face attributes
(setq my-font "Menlo") ;; On Windows: "Consolas"
(setq my-fgcolor "#303030");; "#b9b9b9" ;; "#cccccc"
(setq my-bgcolor "#b9b9b9") ;; "#303030") ;; "#303030


;; Set default frame attributes and initial position
(setq my-frame-alist `((width . 90)
		       (height . 65)))

;; You can specify geometry-related options for the initial frame,
;; however they won't take effect until Emacs reads `.emacs', which
;; happens after first creating the frame. Therefore, set the selected
;; frame's position instead.
(setq default-frame-alist (append my-frame-alist default-frame-alist))
(setq initial-frame-alist (append my-frame-alist initial-frame-alist))

(set-frame-position (selected-frame) 400 40)

;; Set face attributes for existing faces (t: default for all frames)
(set-face-attribute 'default t
                    :background my-bgcolor
                    :foreground my-fgcolor
                    :family my-font
                    :height 130)

(set-face-attribute 'variable-pitch t
                    :family my-font)

(set-face-attribute 'fixed-pitch t
                    :family my-font)

(set-face-attribute 'border t
                    :background "#999999") ; "#454545"

(set-face-attribute 'vertical-border t
                    :foreground "#999999") ; "#454545")

(set-face-attribute 'cursor t
                    :background "#ecdc6b"
                    :foreground "Black")

(set-face-attribute 'fringe t
                    :background my-bgcolor
                    :foreground "#555555")

(set-face-attribute 'highlight t
                    :background "gray75") ; "#3f3f3f"

(set-face-attribute 'header-line t
                    :inherit 'mode-line
                    :background "#a9a9a9"
                    :foreground "#555555"
                    :box nil)

(set-face-attribute 'info-header-xref t
                    :inherit 'info-xref :foreground "LightSteelBlue")

(set-face-attribute 'info-node t
                    :foreground "DeepSkyBlue"
                    :slant 'italic
                    :weight 'bold)

(set-face-attribute 'mouse t
                    :background "Red")

(set-face-attribute 'region t
                    :background "grey75")

(set-face-attribute 'secondary-selection t
                    :background "grey79")

(set-face-attribute 'mode-line t
                    :background "#999999"; "#444444" 
                    :foreground "#555555"
                    :box '(:line-width 1 :color "#999999")) ; "#454545"

(set-face-attribute 'mode-line-inactive t
                    :background "#999999"  ; "#444444"
                    :foreground my-bgcolor
                    :box '(:line-width 1 :color "#999999")) ; "#454545"

(set-face-attribute 'mode-line-highlight nil)

(set-face-attribute 'minibuffer-prompt t
                    :weight 'bold
                    :foreground "gray30")

