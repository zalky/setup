(require 'boot.repl)

(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.15.1"]
                [refactor-nrepl "2.3.1"]])

(swap! boot.repl/*default-middleware* conj
       'cider.nrepl/cider-middleware
       'refactor-nrepl.middleware/wrap-refactor)
