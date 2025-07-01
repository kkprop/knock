(ns knock.tui
  (:require [knock.utils :as utils :refer :all]
            [bblgum.core :as b]
            [progrock.core :as pr]
            [babashka.process :as prc]
            [clojure.string :as str]))


;;bblgum passing direct params using [msg]

(defn res-xs [m] (-> m :result ) )
(defn b-res [m] (-> m :result first))


(defn file []
  (b-res (b/gum :file))
  )

(defn i-filter [f]
  (b-res (b/gum :filter :in (clojure.java.io/input-stream f))))


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
    )
  )

(defn confirm! [msg]
  (let []
    ;(println msg)
    (= 0 (:status (b/gum :confirm [msg] :default "no"))))
  )

(defn yes-confirm! [msg]
  (let []
                                        ;(println msg)
    (= 0 (:status (b/gum :confirm [msg] :default "yes"))))
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
  ([xs]
   (choose! (fn [x] true) xs)
   )
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


(defn input
  ([placeholder]
   (input "" placeholder)
   )
  ([prompt placeholder]
   (-> (b/gum :input :prompt prompt :placeholder placeholder)
       :result
       first
       )
   )
  )



(defn kill-all-gum-pid []
  (kill-cur-pid-by-name  "gum")
  )

(defn render [xs result-fn]
  (let [t (Thread. (fn []
                     (try 
                       ;;still not working.
                       ;;TODO how to fix ctrl c ignored
                       ;;(utils/trap-exit)
                       ;(while true
                         ;(repeatedly (count xs) println)
                         (choose! result-fn xs)
                         ;)
                       (catch Exception e
                         ;;ignore exception
                         (kill-all-gum-pid)
                         ;(println "got exection " e)
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


(defn reg-key [k f]

  )





(comment

  (read)

  (utils/go! (confirm! "try type" ))


  (render [:a :b :c] #(println %))

  )



(comment


  (def p (promise))

  (deliver :p :done)

  (..spit :spin (cur-time-str))
  (go! 
    (spin :spin)
    )
  )

(defn clean-screen []
  (let []
    (kill-all-gum-pid)
    (kill-cur-pid-by-name "tail")
    )
  )

(defn clear-screen []
  "Clear the terminal screen"
  (print "\033[H\033[2J")
  (flush))

(trap-exit (fn []
             (let []
             ;(println (.spit :exiting true))
             ;(println (.slurp :exiting))
             (.local-save)
             (clean-screen))
             ))


(defn .spin [k]
  (let [p (promise!)]

    ;;debug subscribe a file
    ;(async-fn (fn [x] (..spit :spin x))  (tail-f "/tmp/1.txt"))

    (.watch k (fn []
                     ;(println "got new spin value" (..slurp .x))
                     (clean-screen)
                     ))
    ;;keep spinning 
    (go!
      (while (not (realized? p))
        (if (empty? (.slurp k))
          (b/gum :spin ["tail" "-f" "/dev/null"])
           (b/gum :spin ["tail" "-f" "/dev/null"] :title (.slurp k))
          )
        )
      )
    p
    )
  )


(defn .choose [k]
  (let [p (promise)]
    (.watch k (fn []
                  (kill-all-gum-pid)
                  ))
    ;;keep spinning 
    (go!
      (while (not (realized? p))
        (if (empty? (.slurp k))
          (do (println "no value to choose waiting")
              (pause-seconds 3)
              )
          (let [res (b/gum :choose (.slurp k))
                _ (println res)
                ]
            (case (:status res)
              0 (deliver p (first (:result res)))
              ;;user quit
              1 (deliver p nil)
              137 (do "killed by refreshing"
                      nil)
              nil
              )
            )
          )
        )
      )
    p
    )
  )

;;[0 100]
(defn set-bar-progress [n]
  ;(def n 23)
  (.spit :tui-bar-progress n)
  )

(defn set-bar-total [n]
  (.spit :tui-bar-total n)
  )

(defn tick-bar-progress []
  (let [cur (force-int (.slurp :tui-bar-progress))]
    (if (nil? cur) 
      (.spit :tui-bar-progress 0)
      (.spit :tui-bar-progress (+ 1 cur)  )
      )
    )
  )


(defn init-bar []
  (let [k :bar-refresh]
     (set-bar-progress 0)
     (set-bar-total 100)
     (.watch :tui-bar-progress (fn [] (.spit :bar-refresh true)))
     )
  )

(defn .bar []
  (let [p (.spin :tui-bar)
        _ (init-bar)
        ]
    (go! 
      (while (not (realized? p))
        (when (.slurp :bar-refresh )
          (let [bar (-> (pr/progress-bar (.slurp :tui-bar-total))
                        (assoc-in [:progress] (.slurp :tui-bar-progress) )
                        )]
            (if (= (:progress bar) (:total bar))
              (do (.spit :tui-bar (str "✅" (pr/render (pr/done bar))))
                  (pause-seconds 1)
                  (deliver p :done)
                  (clean-screen)
                  )
              (.spit :tui-bar (pr/render bar))
              ))
          (.spit :bar-refresh false)
          )
        (pause-seconds 1)
        ))
    p
    )
  )

(defn .bar!! []
  @(.bar)
  )

(comment

  (def n 0)

  (go! (.bar))

  ..cache
  (set-bar-progress 42)

  (go!
    (let [p (.choose :choose )]
      (println @p)
      )
    )

  (cur-child-by-name "gum")


  (def k :choose)
  @..kk
  @..cache
  (.spit :tui-bar-progress 23)
  (.spit :choose [:a :b ])
  (.spit :choose [:a :b (cur-time-str) ])

  )


(defn .worker! [f coll]
  (worker! (fn [x]
             (let []
               (tick-bar-progress)
               (f x)
               )
             )
           coll
           )
  )

(defn .pipeline!! [n f coll]
  (let []
    (.bar)
    (pipeline!! .worker! n f coll)
    )
  )



(defn demo-pipeline-bar []
  (println (.pipeline!!
             2
             #(do (pause-seconds 0.1) %)
             (range 100)
             ))
  )

(comment

  (tick-bar-progress)

  (prc/tokenize)
  )
