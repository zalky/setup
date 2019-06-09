;;;; Configuration for system backup

(defvar system-credential-directories
  '("~/.2xauth"
    "~/.aws"
    "~/.passwords"
    "~/.ssh"
    "~/.gnupg"))

(defvar system-backup-directories
  '("~/.emacs.d"
    "~/.gitconfig"
    "~/Desktop"
    "~/Documents"
    "~/Downloads"
    "~/Library"
    "~/Movies"
    "~/Music"
    "~/Pictures"
    "~/local"
    "~/opt"))

(defun add-home (files)
  "Given a filename, adds home ~/."
  (mapcar (lambda (s) (concat "~/" s)) files))

(defvar rsync-base-cmd
  "rsync -avvz --partial --progress --delete ")

(defun set-rsync-backup-options ()
  (set (make-local-variable 'comint-output-filter-functions) 'comint-truncate-buffer)
  (set (make-local-variable 'comint-buffer-maximum-size) 50000)
  (set (make-local-variable 'comint-scroll-show-maximum-output) t))

(defun rsync-backup (&optional files)
  "Backs up marked directories."
  (interactive)
  (let* ((rsync-buffer (generate-new-buffer-name "*Rsync Process*"))
         (files (or files
                    (dired-get-marked-files)
                    system-backup-directories))
         (target (read-directory-name "Enter backup folder: "))
         (rsync-cmd (concat rsync-base-cmd (string-join files " ") " " target)))
    (with-current-buffer-window rsync-buffer 'display-buffer-pop-up-window nil
      (message "Starting async backup process to %s..." target)
      (async-shell-command rsync-cmd rsync-buffer "*Messages*")
      (set-rsync-backup-options))))
