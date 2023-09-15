
(ns knock.poker
  (:require [knock.utils :as utils :refer :all]
            [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn default-mapping [m]
  )

(config :card-suites)


(def card-nums
  (range 1 14)
  )

(defn mark[xs & {:keys [id]
               :or {id :id}}]
  (map (fn [x] {id x}) xs)
  )

(defn card-alias [x]
  (get
   {1 "A" 11 "J" 12 "Q" 13 "K"}
   x
   (str x)))

(defn card-num [x]
  (get {1 [1 14]}
       x
       x
       )
  )

(defn suite-alias [x]
  (get card-suites (keyword x))
  )

;;自动获取函数名字
(defn link [xs f & {:keys [name]}]
  (->> xs
       (map #(assoc % name (f (:id %))))))

(defn merge-id [m1 m2]
  (let [new-id (str/join [(:id m1) (:id m2)])]
    (assoc (merge m1 m2) :id new-id)))

(defn comb-merge [coll1 coll2]
  (mapcat (fn [x]
            (map #(merge-id x %) coll2)) coll1))

(def poker
  (comb-merge
    (-> (mark card-nums)
        (link card-alias :name :alias)
        (link card-num :name :num))
   (link (mark
           (keys card-suites))
         suite-alias :name :suite)
   ))

(defn rand-n [coll & xs]
  (let [cards (take (apply + xs) (shuffle coll))]
    (loop [res [] cards cards ii xs]
      (let [n (first ii)]
        (if (empty? ii)
          res
          (recur (conj res (take n cards))
                 (drop n cards)
                 (rest ii)))))
    )
  )

(defn- deal-player [{:keys [players cards]}]
  )
;;debug
(def n-player 3)

(defn count-run [run-point]
  {:pre-flop 5
   :flop 2
   :turn 1
   :river 0
   }
  )

(defn draw [& xs]
  (flatten
   (apply rand-n poker xs)))


(defn deal-in [n-player & {:keys [run-point run-time]
                 :or {run-point :pre-flop
                      run-time 0}
                 :as table
                 }]
  (let [table (assoc table
                     :players (mark (range n-player))
                     :run-time run-time
                     :run-point run-point
                     )
        ]
    (interleave [] :p-cards :burn-pre-flop :pre-flop :burn-turn :turn :burn-river :river 
                (draw (* 2 n-player) 1 3 1 1 1 1)
                )
    )
  )

(comment
  (deal-in 2 {:run-point :flop})
  ;;
  )


(defn print-vals [xs]
  (println (str/join " "
                     (->> xs
                          (map #(dissoc % :id :num))
                          (map vals)
                          (map #(apply str %))
                          ))))

(defn play []
  (let [_ (println (apply str (repeat 80 "-")))
        _ (println (utils/cur-time-str))]
    (apply list
           (->> (rand-n poker 2 2 5)
                (map print-vals))))
  (println (apply str (repeat 80 "-"))))

(defn nums [& xs]
  (flatten
   (map :num xs)
   )
  )

(defn pick-suite [& xs])

(defn offset [xs & {:keys [k]
                    :or {k :num}}]
  (let [front (drop-last xs)
        next (rest xs)]
    (map (fn [a b]
           (- (k a)
              (k b)) ) next front)))

(defn consecutive [xs & {:keys [k]
                         :or {k identity}}]
  (let [raw (->> xs
                 (mapcat #(mapcat-key % k))
                 (sort-by k)
                 (partition-by k))]
    raw
    )
    ;;
  )
;;generate several straight path
(defn offset-consecutive [xs & {:keys [k]
                                :or {k :num}}]
  (let [raw (consecutive xs :k k)
        ;;n-to-obj (->> raw (map (fn [mxs] {(get (first mxs) k) mxs})))
        paths (apply flat-cartesian-product raw)]
    ;;(offset nxs)
    (->> paths
         (map (fn [xs]
                {:offset (offset xs :k k)
                 :xs xs}
                ))
         )
;;
    ))

(defn holdem [xs]
  (let [by-num (consecutive xs :k :num)
        by-suite (consecutive xs :k :suite)]
    ;;
    )
  )

;; count to xs
;; offset consecutive count to xs

(comment
  (straight
   (mock draw 7)
   (mock draw 10)
   (mock-clean draw 7))
  (offset-consecutive (mock draw 7))

  (map :num
       (consecutive
        (mock draw 7) :k :num))

;;
  )

(defn straight [& xs]
  (let []
    (->> (offset-consecutive xs :k :num)
         (map #(let [m (consecutive (:offset %))
                        ones (get m 1)]
                    ;;(< 3 (count ones))
                 ones
                    )
                 )
         )))
(defn flush [& xs]
  (map :suite xs)
  )

(defn straight-flush [& xs])

(defn quads [& xs]
  (let [oxs
        (->>
         (apply nums xs)
         sort
         offset)]
    oxs 
    ))

(comment

  (quads {:num 8}  {:num 8} {:num 8} {:num 8} {:num 10} {:num 10})
  (quads {:num 8} {:num 2} {:num 8} {:num 8} {:num 8} {:num 10} {:num 10})

  (apply straight
         (do
           (mock-clean draw 7)
           (mock draw 7)))

  (map :num
       (mock draw 7))

  ;;this is bad.
  (straight {:num 2} {:num 4} {:num 6} {:num 8} {:num 10})

  (apply straight (mock draw 7))

  (utils/cur-time-str)
     ;;(rand-n poker 2 2 1 3 1 1 1 1)
   ;;
  (mark card-nums)
  (mark card-nums :mark :数字)

  ;;
  )
