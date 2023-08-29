(ns knock.poker
  (:require [knock.utils :as utils :refer :all]
            [clojure.string :as str]
            [clojure.pprint :as pprint]))

(defn default-mapping [m]

  )

(config :suites)


(def nums
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
(defn suite-alias [x]
  (get suites (keyword x))
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
   (link (mark nums) card-alias :name :alias)
   (link (mark (keys suites)) suite-alias :name :suite)
   )
  )



(defn rand-n [coll & xs]
  (let [cards (take (apply + xs) (shuffle coll))]
    (loop [res [] cards cards ii xs]
      (let [n (first ii)]
        (if (empty? ii)
          res
          (recur (conj res (take n cards))
                 (drop n cards)
                 (rest ii)))))))

(defn print-vals [xs]
  (println (str/join " "
                     (->> xs
                          (map #(dissoc % :id))
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


(comment

  (utils/cur-time-str)
     ;;(rand-n poker 2 2 1 3 1 1 1 1)
   ;;
  (mark nums)
  (mark nums :mark :数字))



