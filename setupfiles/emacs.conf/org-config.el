;;;; Org Mode Configuration

;; Load Org Mode
(require 'org)

;; Set Org Mode agenda files from which to collect information. List
;; can be individual files or contain directories.
(setq org-agenda-files (list "~/Documents/Work/Semion/org/"))

;; Load Markdown exporter automatically with Org Mode
(eval-after-load "org"
  '(require 'ox-md nil t))

;; Save Org clock history across Emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Set default Org columns in column view
(setq-default org-columns-default-format
              "#+COLUMNS: %30ITEM(Task) %TODO %DEADLINE(Due) %Effort{:} %CLOCKSUM(Time Spent) %TAGS")

(setq-default org-startup-indented t)

;; Format string used when creating CLOCKSUM lines and when generating
;; a time duration (avoid showing days)
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; By default insert clock keywords + timestamp into :LOGBOOK: drawer
(setq-default org-clock-into-drawer t)

(setq org-log-done t)
