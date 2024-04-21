(ns knock.block
  (:require [knock.utils :refer :all]
            [clojure.string :as str]))

;;block is the fundamental component of tree shape data
:block/string

:block/refs

:block/children

;;drive the process of using blokcs by inspecting this file itself.


(defn block [s]
  {:block/string s}
  )


;; a charactor for chinese
;; a word for other language
(defn ->units [x]
  (let [s (str-or-file x)]
    (->>
     (str/split-lines s)
     (mapcat seq)
     )
    ;;
    ))

(defn nx [n f xs]
  ((apply comp (repeat n f))
   xs
   ))

(defn ->phrase
  ([i n xs]
   (let  [m (map-on-val count (group-by identity
                                        (partition-all n (nx i rest xs))))]

     m))
  ([n xs]
   ;;first
   (let [m (->> (range n)
                (map #(->phrase % n xs))
                (apply merge-with +))
         mm (map-on-key #(apply str %) m)]
     (into (sorted-map-by (fn [a b] (compare
                                     [(get mm b) b]
                                     [(get mm a) a]))) mm)))

  ([xs]
   (->phrase 1 xs)))

(defn ->semantics[]
  )

(comment
  (pp
   (sorted-by-val
    (->phrase 1 3
              (->units "说你又不听，听你又不懂，懂你又不做，做你又做错，错你又不认，认你又不服，不服你又不出声"
                ;(->abs-path "~/shurangama")
                       ))
    )
   )

  (pp
   (->phrase 2 (->units "蜕变： 不喝酒了改喝茶了， 不抽烟了改熏香了， 不听歌了改听戏了， 专心烧香拜佛和赚钱")))

  (->units "The Limits of my language are the limits of my world")

  (run-cmd :clear)
  ;;
  )
