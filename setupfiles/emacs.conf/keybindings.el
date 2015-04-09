;;;; Global Key Bindings

;; Global keymaps can be done using `global-set-key', which takes
;; "\key" and 'command as arguments. Mode specific keymaps are bound
;; using the `define-key' function, with the specific keymap, "\key"
;; and 'command as arguments.
(define-key global-map "\C-z" 'undo)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-x 9") 'transpose-buffers)

;; Change mac command keys
(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)
