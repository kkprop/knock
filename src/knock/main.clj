(ns knock.main
  (:gen-class)
  (:require [knock.wn :as wn]
            [clojure.tools.cli :refer [parse-opts]]
            )
)

(def cli-options
  ;; An option with a required argument
  [["-h" "--help"]
   ["-s" "--search WORD" "a word to search"]
    ])

(defn -main [& args]
  (let [cli (parse-opts *command-line-args* cli-options)
        options (:options cli)
                 ]
    (cond
      (:search options)
      (wn/search-wn (:search options)) 

      :default
      (println cli)
      )
))
