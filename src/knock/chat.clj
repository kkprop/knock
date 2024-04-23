(ns knock.chat
  (:require [knock.utils :refer :all]
            [babashka.curl :as curl]
            [babashka.process :as p]
            [babashka.fs :as fs]))

(config :model-dir :default (->abs-path "~/models"))

(defn models []
  (map str (fs/glob model-dir "*.gguf")))

(defn prompt [q]
  (run-cmd (str model-dir "/bin/main -m")
           (first (models))
           "-c 4096 --temp 0.7 --repeat_penalty 1.1 "
           ;;suppress log
           "2>/dev/null"
           ;;input text
           "-p" (str  "'" q "'")))

(defn prompt![p]
  (run-cmd (str model-dir "/bin/main -m")
           (first (models))
           "-c 4096 --temp 0.7 --repeat_penalty 1.1 "
           ;;suppress log
           "2>/dev/null"
           ;;input text
           "-ins" ))



(comment
  ;;
  (:out
    (prompt "what is socrate question method when doing inquery?")
    )
;
)
