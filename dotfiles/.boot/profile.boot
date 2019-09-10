(require 'boot.repl
         'clojure.java.io)

(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.22.1"]
                [refactor-nrepl "2.4.0"]])

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
