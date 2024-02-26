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
    (count (slurp-lines x))
    ;;
    ))

(comment
  (->units
    (->abs-path "~/xj")
    )
  ;;
  )
