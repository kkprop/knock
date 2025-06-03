(ns knock.chat
  (:require [knock.utils :refer :all]
            [babashka.curl :as curl]
            [knock.tui :as tui :refer :all]
            [clojure.string :as str]
            [babashka.process :as proc]
            [babashka.fs :as fs]
            )
  )

;(config :model-dir :default (->abs-path "~/models")) ;(def input-file "/tmp/input-for-models.pid")
;
;(defn models []
;  (map str (fs/glob model-dir "*.gguf"))
;  )
;
;(defn ->model-cmd [path]
;  (apply join-cmd (str model-dir "/bin/main -m")
;         path
;         "-c 4096 --temp 0.7 --repeat_penalty 1.3 "
;         ;;suppress log
;         ;;"2>/dev/null"
;         ;;instruction first
;         "-ins -r '###'"
;         []
;         )
;  )
;
;
;(defn run-model []
;  (let [_ (touch input-file)]
;    (let [xs (->> (models)
;                  (remove pid-running?))
;          m (tui/choose (models) 10)
;          w (-> m
;                basename
;                file-name
;                (str/replace "." "-")
;                (str "-gpt"))]
;      (thread!
;       (try
;         (run-shell "tmux new-session -s" w (str "\"" (->model-cmd m) "\""))
;         (catch Exception :as e
;                (println  "run: " "tmux kill-session -t " w " \n clean the old tmux session")
;                )))
;      (thread!
;       (async-fn (fn [x]
;                   (println x)
;                   ;;check last character > ? 
;                   (when-not (str/starts-with? (:out (run-cmd "tmux capture-pane -t" w "-p | tail -n 1")) ">")
;                     (send-keys w "c-c"))
;                   (send-text w x)
;                   (send-keys w "Enter"))
;                 (tail-f input-file)))
;      @(promise))))
;
;(comment
;  (parse-md-file "resources/ask-suffix.yaml")
;  )
;
;(defn all-gpt-tmux []
;  (let [xs (str/split-lines (run-cmd! :tmux "list-windows -a"))]
;    (->> xs
;         (map #(first (str/split % #":")))
;         (filter #(str/ends-with? % "-gpt")))))
;
;
;
;(defn prompt [s]
;  (run-cmd :echo s ">>" input-file)
;  )
;
;(defn clipboard-prompt []
;  (loop [prev nil]
;    (let [cur (run-cmd! :pbpaste)]
;      (if (= cur prev)
;        (Thread/sleep 100)
;        (do
;          (println (cur-time-str) " sent:" cur)
;          (prompt cur)))
;      (recur cur))))
;
;(defn capture-stdin []
;  (let [_ (touch input-file)
;        old *in*]
;    (with-open [r (clojure.java.io/reader input-file)]
;      (binding []
;        ;;hijack *in*
;        (thread! (proc/shell "tmux new-session -d -s dc "))
;        (thread! (Thread/sleep 1000) (proc/shell "tmux send-keys -t dc Enter"))
;        @(promise)))))


;;on ollama
;;curl http://localhost:11434/api/tags

(def url-ollama "http://localhost:11434/api/")

(defn ollama
  ([path]
   (ollama path nil))
  ([path opts]
   (let []
     (case path
       "tags" (:body (curl-get (make-url url-ollama path)))
       (:body (curl-post (make-url url-ollama path)
                         :body opts
                         :as :stream
                         ))
;;
       ))))

(defn models
  ([]
   (map :name (:models (mock! ollama "tags")))))



(defn chat
  ([msg]
   (let [m (last (mock models))]
     (chat m msg)))

  ([m msg]
   (ollama "chat"
           {:model m
            :messages [{:role "user" :content msg}]})
   )
  )

(defn last-model []
  )

(def qa-cache (atom {}))

;; TODO: collect Q&A pairs
(defn print-chat
  ([msg]
   (let [m (last (models))]
     (print-chat m msg)))

  ([m msg]
   (let [id (->uuid msg)
         _ (swap! qa-cache assoc id {:q msg})]

     (async-fn #(let [m (->edn %)
                      x (-> m :message :content)]
                ;;flush when got res
                  (when (or  (str/ends-with? x "\n")
                             (str/ends-with? x ".")
                             (str/starts-with? x " "))
                    (flush))
                  (when (:done m)
                    (println "")
                    (println bub)
                    ;;show the prev Q&A
                    (pp (get @qa-cache id)))
                  (swap! qa-cache merge {id {:q msg
                                             :a
                                             (str (:a (get @qa-cache id))
                                                  x)}})
                  (print x)
                  ;;
                  )
               ;;convert InputStream into a channel
               (->ch (chat
                       ;;add all in the cache as context
                       ;;join together
                      (str/join "\n"
                                (flatten
                                  ;;previous questions
                                 (conj
                                  (map :q (vals @qa-cache))
                                  ;;current 
                                  msg
                                  )
                                 )
                                )
                      )))
     )
   )
  )

;;This is not good.
(defn run-model []
  (let [m (tui/choose (models) 10)]
    (while true
      (print "> ")
      (flush)
      (print-chat m
                  (let [s (read-line)]
                    (if (str/starts-with? s "@")
                      (str "加载一下上下文："
                           (slurp (chop-leading-n s 1))
                           )
                      s)))

      (println "")
      (print "> "))
    ;;
    ))



(comment
  (print-chat "what is space?")

  (prompt "什么是时间？なに")
  (prompt "如何提升跑步配速？なに")

  (pid-file "gguf")

  (def xs (prompt!))
  ;;
  (:out
   (prompt "what is socrate question method when doing inquery?"))
;
  )




;; stdout stdin 

(defn o []
  (go! (tmux "ollama"))

  (send-text  (->uuid "ollama") "ollama run deepseek-r1:8b")

  (send-keys (->uuid "ollama")
             "Enter"
             ))



(comment


  (def p
    (proc/exec "ollama" "run" "deepseek-r1:8b")
    )

  (def id (tmux "ollama")
    )
  ;;
  )
