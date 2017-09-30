
(setq js-indent-level 2)

(defun disable-company-mode ()
  (company-mode -1))

(add-hook 'indium-repl-mode-hook 'disable-company-mode)
