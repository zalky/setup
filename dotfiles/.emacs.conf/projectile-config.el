;;;; Projectile Configuration

;; projectile everywhere!
(require 'helm-projectile)

(projectile-global-mode)

(setq projectile-completion-system 'helm)

(helm-projectile-on)

;; When switching projects, show buffers not files.
(setq projectile-switch-project-action 'helm-projectile-switch-to-buffer)

(setq grep-find-ignored-directories
      (append '("target" "out") grep-find-ignored-directories))

