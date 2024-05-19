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
         ;;instruction first
         "-ins -r '###'"
         []
         )
  )

(defn send-text [id & xs]
  (run-cmd "tmux send-keys -t" id "'" (str/join " " xs) "'"))

(defn send-keys [id & xs]
  (apply run-cmd "tmux send-keys -t" id xs))

(defn run-model []
  (let [_ (touch input-file)]
    (let [xs (->> (models)
                  (remove pid-running?))
          m (choose (models) 10)
          w (-> m
                basename
                file-name
                (str/replace "." "-")
                (str "-gpt"))]
      (thread! (run-shell "tmux new-session -s" w (str "\"" (->model-cmd m) "\"")))
      (thread!
       (async-fn (fn [x]
                   (println x)
                   ;;check last character > ? 
                   (when-not (str/starts-with? (:out (run-cmd "tmux capture-pane -t" w "-p | tail -n 1")) ">")
                     (send-keys w "c-c"))
                   (send-text w x)
                   (send-keys w "Enter"))
                 (tail-f input-file)
                 ))
      @(promise))))

(comment
  (parse-md-file "resources/ask-suffix.yaml")
  )

(defn all-gpt-tmux []
  (let [xs (str/split-lines (run-cmd! :tmux "list-windows -a"))]
    (->> xs
         (map #(first (str/split % #":")))
         (filter #(str/ends-with? % "-gpt")))))



(defn prompt [s]
  (run-cmd :echo s ">>" input-file)
  )

(defn clipboard-prompt []
  (loop [prev nil]
    (let [cur (run-cmd! :pbpaste)]
      (if (= cur prev)
        (Thread/sleep 100)
        (do
          (println (cur-time-str) " sent:" cur)
          (prompt cur)))
      (recur cur))))

(defn capture-stdin []
  (let [_ (touch input-file)
        old *in*]
    (with-open [r (clojure.java.io/reader input-file)]
      (binding []
        ;;hijack *in*
        (thread! (proc/shell "tmux new-session -d -s dc "))
        (thread! (Thread/sleep 1000) (proc/shell "tmux send-keys -t dc Enter"))
        @(promise)))))

(comment
  (prompt "what is space?")
  (prompt "什么是时间？なに")
  (prompt "如何提升跑步配速？なに")


  (pid-file "gguf")

  (def xs (prompt!))
  ;;
  (:out
   (prompt "what is socrate question method when doing inquery?"))
;
  )
