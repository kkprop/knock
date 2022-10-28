(ns knock.main
  (:gen-class)
  (:require [knock.wn :as wn]
            [knock.roam :as r]
            [knock.utils :as utils :refer :all]
            [clojure.tools.cli :refer [parse-opts]]
            )
)

(def cli-options
  ;; An option with a required argument
  [["-h" "--help"]
   ["-s" "--search WORD" "a word to search"]
   ["-w" "--write things" "things write to roam daily note"]
    ])

(defn -main [& args]
  (let [cli (parse-opts *command-line-args* cli-options)
        options (:options cli)
                 ]
    (cond
      (:search options)
      (wn/search-wn (:search options)) 

      (:write options)
      (r/write (load-edn "Tickers.edn") (:write options)  )

      :default
      (println cli)
      )
))
