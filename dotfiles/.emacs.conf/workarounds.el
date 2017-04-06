;;;; Workarounds

;; Note: There is a line in org-faces.el:
;; (org-copy-face 'mode-line 'org-mode-line-clock-overrun
;;   "Face used for clock display for overrun tasks in mode line.")
;; That sets org-mode-line-clock-overrun to red background. This
;; overrides any customizations being set by user init. This should be
;; removed.

;; Fix node.js prompt in eshell
(setenv "NODE_NO_READLINE" "1")

;; Fix junk characters in shell mode (due to loading .bashrc)
;;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Filter out ansi colour characters
;; (setq-default ansi-color-for-comint-mode 'filter)


;; XFT fix for development snapshot
(push '(font-backend xft x) default-frame-alist)

;; This makes path match shell variable and puts anaconda python in search path
(defun set-exec-path-from-shell-PATH ()
        (interactive)
        (let ((path-from-shell (replace-regexp-in-string "^.*\n.*shell\n" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator))))
 
(set-exec-path-from-shell-PATH)
