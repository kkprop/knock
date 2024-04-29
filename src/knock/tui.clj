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

(defn choose
  ([xs]
   (choose xs 0)
   )
  ;;default seconds
  ([xs timeout]
   (->
    (b/gum :choose xs :timeout (str timeout "s"))
    :result
    first
    )
   )
  )


