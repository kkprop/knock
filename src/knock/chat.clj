(ns knock.chat
  (:require [knock.utils :refer :all]
            [babashka.curl :as curl]
            [knock.tui :refer :all]
            [clojure.string :as str]
            [babashka.process :as proc]
            [babashka.fs :as fs]))

(config :model-dir :default (->abs-path "~/models"))
(def input-file "/tmp/input-for-models.pid")

(defn models []
  (map str (fs/glob model-dir "*.gguf"))
  )

(defn ->model-cmd [path]
  (apply join-cmd (str model-dir "/bin/main -m")
         path
         "-c 4096 --temp 0.7 --repeat_penalty 1.3 "
         ;;suppress log
         ;;"2>/dev/null"
         "-i -r '###'"
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

(defn send-text [id & xs]
  (run-cmd "tmux send-keys" id "'" (str/join " " xs) "'")
  )

(defn send-keys [id & xs]
  (apply run-cmd "tmux send-keys" id xs))


(defn run-model []
  (let [_ (touch input-file)]
    (let [xs (->> (models)
                  (remove pid-running?))
          m (choose (models) 10)]
      (thread! (proc/shell (str "tmux new-session -s dc "
                                    (str "\"" (->model-cmd m) "\""))))
      (thread!
        (async-fn (fn [x]
                    (println "send" x)
                    (send-text "dc" x)
                    (send-keys "dc" "Enter")))
        (tail-f input-file)
        )
      @(promise))))

(defn capture-stdin []
  (let [_ (touch input-file)
        old *in*]
    (with-open [r (clojure.java.io/reader input-file)]
      (binding []
        ;;hijack *in*
        ;(thread! (async-fn (fn [x] (println "got" x) (println "count:" (count x))) (->ch *in*)))
        (thread! (proc/shell "tmux new-session -d -s dc "))
        (thread! (Thread/sleep 1000) (proc/shell "tmux send-keys -t dc Enter"))
        @(promise)))))

(comment
  (pid-file "gguf")

  (def xs (prompt! ))
  ;;
  (:out
    (prompt "what is socrate question method when doing inquery?")
    )
;
)
