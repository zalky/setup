(require 'boot.repl)

(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.16.0"]
                [refactor-nrepl "2.4.0-SNAPSHOT"]])

(swap! boot.repl/*default-middleware* conj
       'cider.nrepl/cider-middleware
       'refactor-nrepl.middleware/wrap-refactor)

;; Ugh, disable printing of namespaced maps.
(defmethod print-method clojure.lang.IPersistentMap
  [m, ^java.io.Writer w]
  (#'clojure.core/print-meta m w)
  (#'clojure.core/print-map m #'clojure.core/pr-on w))
