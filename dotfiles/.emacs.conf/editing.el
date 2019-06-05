;;;; Configuration settings for: Editing Buffers

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; Don't do this, can leak sensitive data.
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Set language environment and input method
(setq current-language-environment "UTF-8")
(setq default-input-method "rfc1345")

;; Set ispell program name
(setq ispell-program-name "aspell")

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Indent with spaces
(setq-default indent-tabs-mode nil)

;; Set default indendation width
(setq-default standard-indent 4)

;; Set default tab width
(setq-default tab-width 4)

;; Automatically indent to proper level after every <RET>. t by default.
(setq electric-indent-mode t)

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;;;  These settings relate to how emacs interacts with your operating system

;; makes killing/yanking interact with the clipboard
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

;; Save clipboard strings into kill ring before replacing them.
;; When one selects something in another program to paste it into Emacs,
;; but kills something in Emacs before actually pasting it,
;; this selection is gone unless this variable is non-nil
(setq save-interprogram-paste-before-kill t)

(defun fill-to-end ()
  "Fills remainder of line with comment start characters"
  (interactive)
  (save-excursion
    (end-of-line)
    (while (< (current-column) 70)
      (insert comment-start))))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position)
                               (line-end-position)))

;; (defun dired-get-size ()
;;   "Asynchronously calculates the size of marked files and
;; displays them plus total to buffer."
;;   (interactive)
;;   (let* ((help-buffer "*Dired Size*")
;;          (files (dired-get-marked-files))
;;          (du-command (concat "/usr/bin/du -sch "
;;                              (string-join files " "))))
;;     (with-output-to-temp-buffer help-buffer
;;       (message "Calculating size asynchronously...")
;;       (async-shell-command du-command help-buffer "*Messages*")
;;       (pop-to-buffer help-buffer))))

;; (defun dired-get-size ()
;;   "Prints the cummulative size of marked files to the
;; minibuffer."
;;   (interactive)
;;   (let ((files (dired-get-marked-files)))
;;     (with-temp-buffer
;;       (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
;;       (message "Size of all marked files: %s"
;;                (progn 
;;                  (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
;;                  (match-string 1))))))

(defun dired-get-size ()
  "Asynchronously calculates the size of marked files and
displays them plus total to buffer."
  (interactive)
  (let* ((files (dired-get-marked-files))
         (du-command (concat "/usr/bin/du -sch "
                             (string-join files " "))))
    (message "Calculating size asynchronously...")
    (async-shell-command du-command)))

(define-key dired-mode-map (kbd "?") 'dired-get-size)
