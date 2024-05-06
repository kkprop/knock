(ns knock.chat
  (:require [knock.utils :refer :all]
            [babashka.curl :as curl]
            [knock.tui :refer :all]
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


(defn run-model []
  (let [_ (touch input-file)]
    (with-open [i (clojure.java.io/input-stream input-file)]
      (let [xs (->> (models)
                    (remove pid-running?))
            m (choose (models) 10)]
        (println m)
        (proc/shell {:in i} (->model-cmd m))
        (thread!
         (async-fn (fn [x]
                     (println x)) (->ch *in*)))
        @(promise))
      )
    ))

(defn capture-stdin []
  (let [_ (touch input-file)
        old *in*]
    (with-open [r (clojure.java.io/reader input-file)]
      (binding [*in* r]
        ;;hijack *in*
        ;(thread! (async-fn (fn [x] (println "got" x) (println "count:" (count x))) (->ch *in*)))
        (thread!
         (async-fn (fn [x]
                     (println "old input got" x))
                   (->ch old)))

        (thread!
         (println "try read line")
         (loop []
           (let [x (read-line)]
             (println "got" x)
             )
           ))
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
