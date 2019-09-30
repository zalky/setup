;;;; Projectile Configuration

;; projectile everywhere!
(require 'helm-projectile)

(projectile-global-mode)

(setq projectile-completion-system 'helm)

;; Workaround for projectile-find-file bug, see:
;; https://github.com/bbatsov/projectile/issues/1302
(setq projectile-git-submodule-command nil)

(helm-projectile-on)

;; When switching projects, show buffers not files.
(setq projectile-switch-project-action 'helm-projectile-switch-to-buffer)

(setq projectile-globally-ignored-directories
      (append '(".git"
                ".svn"
                "out"
                "repl"
                "target"
                "out")
              projectile-globally-ignored-directories))

(setq projectile-globally-ignored-files
      (append '(".DS_Store"
                "*.gz"
                "*.pyc"
                "*.jar"
                "*.tar.gz"
                "*.tgz"
                "*.zip")
              projectile-globally-ignored-files))

(setq grep-find-ignored-directories
      (append '("target"
                "out"
                "backup*"
                "assets"
                "bower_components"
                "data"
                "log")
              grep-find-ignored-directories))

