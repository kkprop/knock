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


(defn run-model []
  (let [_ (touch input-file)
        old *in*]
    (with-open [rdr (clojure.java.io/reader input-file)]
      (binding []
        (let [xs (->> (models)
                      (remove pid-running?))
              m (choose (models) 10)]
          ;(println m)
          (thread!
           (proc/shell (->model-cmd m)))
          ;(thread! (async-fn (fn [x] (println "got " x "len:" (count x))) (->ch *in*)))
          ;(thread! (println "try read line") (while true (when-let [x (read-line)] (println "got" x " len:" (count x))) (Thread/sleep 100)))
          (thread!
            (println "try")
            (Thread/sleep 5000)
            (proc/shell "expect -c 'send -- \\x03'")
            (proc/shell "expect -c 'send -- \\x03'")
            (println "stop here"))
          (thread! (Thread/sleep 6000)
                   (proc/shell "expect -c 'send -- \"什么是时间\r\n\"'")
                   (proc/shell "expect -c 'send -- \"什么是空间\r\n\"'")
                   )
          @(promise))))))

(defn capture-stdin []
  (let [_ (touch input-file)
        old *in*]
    (with-open [r (clojure.java.io/reader input-file)]
      (binding []
        ;;hijack *in*
        ;(thread! (async-fn (fn [x] (println "got" x) (println "count:" (count x))) (->ch *in*)))
        ;(thread! (async-fn (fn [x] (println "old input got" x)) (->ch old)))
        ;;ok to keep reading now 
        ;(thread! (println "try read line") (while true (when-let [x (read-line)] (println "got" x " len:" (count x))) ;(println 'sleeping) (Thread/sleep 100)))
        ;(thread! (proc/shell "gum" "confirm"))
        (thread! (choose [:a :b]))
        (thread! (Thread/sleep 1000) (proc/shell "expect -c 'send -- \"\\x03\"'") (println "send c-c"))
        (thread! (Thread/sleep 200) (proc/shell "expect -c 'send -- \033\\[B'"))
        ;(thread! (Thread/sleep 200) (proc/shell "expect -c 'send -- 什么是时间ha? \n'"))
        ;(thread! (Thread/sleep 200) (proc/shell "expect -c 'send --  \"\n什么是时间? \n\"'"))
        @(promise)))))

(defn ww []
  ;;^C
  (spit input-file (str (char 3)) :append true)
  ;;^L
  (spit input-file (str (char 12)) :append true)
  ;;^M return 
  (spit input-file (str (char 13)) :append true)
  (spit input-file (str (char 0x50)) :append true)
  (spit input-file (str "^[[B") :append true)
  (proc/shell "expect -c 'send -- \n'" )
  (spit input-file (str "what is time? \n" \return \/) :append true)
  ;;
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
