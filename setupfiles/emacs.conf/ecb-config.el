;;;; ecb-config.el ---- Initialize and configure Emacs Code Browser

;;;; Copyright (C) Zalan Kemenczy

;; Author:  Zalan Kemenczy <zalan.k@gmail.com>
;; Created: Feb 12, 2007
;; Revised: Mar 1, 2015
;; Emacs v: 24.4.90.1

;; Require Emacs Code Browser
(add-to-list 'load-path (expand-file-name "~/local/share/elisp/ecb/"))
(require 'ecb)

;; Disable tip of the day
(setq ecb-tip-of-the-day nil)

;; Remove ugly icons from emacs code browser
(setq ecb-tree-buffer-style 'ascii-no-guides)
(setq-default ecb-display-image-icons-for-semantic-tags nil)

(defcustom ecb-display-image-icons-for-semantic-tags nil
  "*Display nice and pretty icons for semantic-tags in the Methods-buffer.
A non nil value takes only effect if Emacs can display images and if
`ecb-tree-buffer-style' is set to 'image."
  :group 'ecb-methods
  :type 'boolean)

;; Set primary mouse click to be left click and Ctrl right click
(setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)

;; Set Emacs Code Browser version
(setq ecb-option-version "2.32")
