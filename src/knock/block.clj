(ns knock.block
  (:require [bblgum.core :as b]))

;;block is the fundamental component of tree shape data
:block/string

:block/refs

:block/children

;;drive the process of using blokcs by inspecting this file itself.


(defn block [s]
  {:block/string s}
  )

(defn gum [& xs]
  (apply b/gum (keyword (first xs)) (rest xs)))

(defn filter []
  (b/gum :filter :in (clojure.java.io/input-stream
                      (->
                       (b/gum :file)
                       :result
                       first
                       )
                      )))
(comment

  )
