;;;; Python specific confuguration settings

(require 'python)

;; Configure Python configuration to use ipython
(setq python-shell-interpreter "ipython")

;; If desired, start ipython in pylab mode. But this auto-imports a
;; bunch of names into global namespace, and convenience is not worth
;; the tradeoff in namespace confusion.
;; (setq python-shell-interpreter-args "--pylab")

