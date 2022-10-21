(ns knock.utils
  (:require [clojure.java.io :as io])
  )

(defn cur-time-str
  ([]
   (cur-time-str "yyyy-MM-dd hh:mm:ss")
   )
  ([fmt]
   (.format (java.text.SimpleDateFormat. fmt) (java.util.Date.))
   )
  )
