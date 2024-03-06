(ns knock.block
  (:require [knock.utils :refer :all]))

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
     (slurp-lines x)
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
                (apply merge))
         mm (map-on-key #(apply str %) m)
         ]
     ;(into (sorted-map-by (fn [a b] (compare (get m b) (get m a)))) m)
     mm
     ))

  ([xs]
   (->phrase 1 xs)))

(comment
  (pp
    (->phrase 5
              (->units
                (->abs-path "~/shurangama"))))
  ;;
  )
