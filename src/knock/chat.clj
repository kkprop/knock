(ns knock.chat
  (:require [knock.utils :refer :all]
            [babashka.curl :as curl]
            [knock.tui :refer :all]
            [babashka.process :as proc]
            [babashka.fs :as fs]))

(config :model-dir :default (->abs-path "~/models"))

(defn models []
  (map str (fs/glob model-dir "*.gguf"))
  )

(defn ->model-cmd [path]
  (apply join-cmd (str model-dir "/bin/main -m")
         path
         "-c 4096 --temp 0.7 --repeat_penalty 1.9 "
         ;;suppress log
         ;;"2>/dev/null"
         "-ins -r '###'"
         []
         )
  )

(defn prompt [q]
  (run-cmd (str model-dir "/bin/main -m")
           (first (models))
           "-c 4096 --temp 0.7 --repeat_penalty 1.1 "
           ;;suppress log
           "2>/dev/null"
           ;;input text
           "-p" (str  "'" q "'")
           )
  )


(defn pid-path [s]
  (str "/tmp/" s ".pid"))

(defn pid-running? [s]
  (fs/exists? (pid-path s) )
  )

(defn pid-file [s]
  (let [path (pid-path s)]
    ;;exist means exit. ignore multiple times create
    (when-not (pid-running? s)
      (fs/create-file path))
    (fs/delete-on-exit path)
    ;;indeed work
    ;(Thread/sleep 1000)
    ))


(defn run-model []
  (let [xs (->> (models)
                (remove pid-running?)
                )
        m (choose (models) 1)]
    (->ch "/tmp/input-for-models.pid")
    (proc/process {:in :inherit :out :inherit
                   :continue true } (->model-cmd m))
    (thread!
      (async-fn (fn [x]
                  ))
      )
    @(promise)
    )
  )


(comment
  (pid-file "gguf")

  (def xs (prompt! ))
  ;;
  (:out
    (prompt "what is socrate question method when doing inquery?")
    )
;
)
