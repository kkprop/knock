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

(defn choose!
  ([result-fn xs]
   (choose! result-fn xs 0)
   )
  ([result-fn xs timeout]
   (let [x (choose xs)]
     (when-not (nil? x)
       (if (result-fn x)
         x
         (do
           (println "retry")
           (utils/pause 1000)
           (choose! result-fn xs))
         ))
     )
   )
  )





(defn kill-all-gum-pid []
  (->> (utils/cur-child-pids)
       (utils/filter! {:COMMAND "gum"})
       (map :PID)
       (map utils/kill-pid!)
       (apply list)
       )
  )

(defn render [xs result-fn]
  (let [t (Thread. (fn []
                     (try 
                       ;(while true
                         ;(repeatedly (count xs) println)
                         (choose! result-fn xs)
                         ;)
                       (catch Exception e
                         ;;ignore exception
                         (kill-all-gum-pid)
                         ))
                       )
                   )]
    (.start t)
    t
    )
  )

(comment

  (utils/show-members (Thread. #(println)))
  (.start t)
  (.interrupt t)


)
