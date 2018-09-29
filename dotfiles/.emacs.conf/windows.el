;;;; Configuration settings for: Navigating Buffers and Windows

;; "Shrink-wrap" frame to buffer size.
;;(require 'fit-frame)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode t)
(setq recentf-max-menu-items 40)

;; Function to swap a buffer with the one in a window bellow it. This function
;; is later bound globally to the key `C-x 9'
(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; transpose-frame.el
(require 'transpose-frame)

;; This program provides some interactive functions which allows users
;; to transpose windows arrangement in currently selected frame:
;;
;; `transpose-frame'  ...  Swap x-direction and y-direction
;;
;;        +------------+------------+      +----------------+--------+
;;        |            |     B      |      |        A       |        |
;;        |     A      +------------+      |                |        |
;;        |            |     C      |  =>  +--------+-------+   D    |
;;        +------------+------------+      |   B    |   C   |        |
;;        |            D            |      |        |       |        |
;;        +-------------------------+      +--------+-------+--------+
;;
;; `flip-frame'  ...  Flip vertically
;;
;;        +------------+------------+      +------------+------------+
;;        |            |     B      |      |            D            |
;;        |     A      +------------+      +------------+------------+
;;        |            |     C      |  =>  |            |     C      |
;;        +------------+------------+      |     A      +------------+
;;        |            D            |      |            |     B      |
;;        +-------------------------+      +------------+------------+
;;
;; `flop-frame'  ...  Flop horizontally
;;
;;        +------------+------------+      +------------+------------+
;;        |            |     B      |      |     B      |            |
;;        |     A      +------------+      +------------+     A      |
;;        |            |     C      |  =>  |     C      |            |
;;        +------------+------------+      +------------+------------+
;;        |            D            |      |            D            |
;;        +-------------------------+      +-------------------------+
;;
;; `rotate-frame'  ...  Rotate 180 degrees
;;
;;        +------------+------------+      +-------------------------+
;;        |            |     B      |      |            D            |
;;        |     A      +------------+      +------------+------------+
;;        |            |     C      |  =>  |     C      |            |
;;        +------------+------------+      +------------+     A      |
;;        |            D            |      |     B      |            |
;;        +-------------------------+      +------------+------------+
;;
;; `rotate-frame-clockwise'  ...  Rotate 90 degrees clockwise
;;
;;        +------------+------------+      +-------+-----------------+
;;        |            |     B      |      |       |        A        |
;;        |     A      +------------+      |       |                 |
;;        |            |     C      |  =>  |   D   +--------+--------+
;;        +------------+------------+      |       |   B    |   C    |
;;        |            D            |      |       |        |        |
;;        +-------------------------+      +-------+--------+--------+
;;
;; `rotate-frame-anticlockwise'  ...  Rotate 90 degrees anti-clockwise
;;
;;        +------------+------------+      +--------+--------+-------+
;;        |            |     B      |      |   B    |   C    |       |
;;        |     A      +------------+      |        |        |       |
;;        |            |     C      |  =>  +--------+--------+   D   |
;;        +------------+------------+      |        A        |       |
;;        |            D            |      |                 |       |
;;        +-------------------------+      +-----------------+-------+

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
