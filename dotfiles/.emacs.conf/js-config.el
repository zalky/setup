
(require 'js2-mode)
(require 'indium)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq js-indent-level 2)
(setq js2-strict-missing-semi-warning nil)

(defun disable-company-mode ()
  (company-mode -1))

(add-hook 'indium-repl-mode-hook 'disable-company-mode)

(setq coffee-tab-width 2)

