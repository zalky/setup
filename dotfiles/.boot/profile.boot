(require 'boot.repl
         'clojure.java.io)

(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.25.9"]
                [refactor-nrepl "2.5.1"]
                [cider/piggieback "0.5.2" :scope "test"]])

(swap! boot.repl/*default-middleware* conj
       'cider.nrepl/cider-middleware
       'refactor-nrepl.middleware/wrap-refactor)

;; Get credentials from a GPG encrypted file.
;; (configure-repositories!
;;  (let [home-dir (System/getProperty "user.home")
;;        creds-file (clojure.java.io/file home-dir ".lein/credentials.clj.gpg")
;;        creds-data (gpg-decrypt creds-file :as :edn)]
;;    (fn [{:keys [url] :as repo-map}]
;;      (merge repo-map (creds-data url)))))
