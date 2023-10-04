(ns lucy.api
  (:require [lucy.data :as data :refer :all]
            [knock.utils :as utils]))

(utils/config :local-roam-edn :default "./1.edn")

(defn rand []
  (println (count @data/dbs))
  (rand-block
   (data/db local-roam-edn)
   )
 )

(comment
  (rand )
  (time
   (count
    (data/load-roam-edn local-roam-edn))
   )
  (time
   (count
    ("/Users/dc/Documents/Roam/1.edn" :readers {'datascript/DB identity}))))
