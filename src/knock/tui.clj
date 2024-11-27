(ns knock.tui
  (:require [knock.utils :as utils]
            [bblgum.core :as b]
            [clojure.string :as str]))


(defn res-xs [m] (-> m :result ) )
(defn res [m] (-> m :result first))


(defn file []
  (res (b/gum :file))
  )

(defn i-filter [f]
  (res (b/gum :filter :in (clojure.java.io/input-stream f))))


;; !! side effect the content choose will be copy to clipboard 
(defn search [xs to-choose & to-search]
  (let [m (if (nil? to-search)
            (utils/searchablize xs to-choose)
            (apply (partial utils/searchablize xs to-choose)
                   to-search))
        xs (str/join "\n" (keys m))
        k (i-filter (utils/tmp-file xs))
        x (get m k)
        ]
    (utils/run-cmd :echo x "| pbcopy")
    x
    )
  )

(def tc (atom 0))

(defn tc! []
  (let [res @tc]
    (swap! tc inc)
    res
    )
  )

(defn confirm? [& xs]
  "if options appointed. will choose them one by one"
  (if (empty? xs)
    (= 0 (:status (b/gum :confirm)))
    (let [i (mod (tc!) (count xs))] 
      (nth xs i )
      )
    ))


(comment 
  (confirm? true false)

  )

