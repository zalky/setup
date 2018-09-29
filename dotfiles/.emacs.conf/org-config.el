;;;; Org Conifuration

;; Load Org Mode
(require 'org)

;; Org-mode shortcuts
(define-key org-mode-map (kbd "C-M-n") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "C-M-p") 'outline-previous-visible-heading)
(define-key org-mode-map (kbd "C-c l") 'org-store-link)
(define-key org-mode-map (kbd "C-c a") 'org-agenda)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

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
