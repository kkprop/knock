(ns lucy.local-server
  (:require [knock.server :as server]
            [lucy.api :as api]
            [clojure.data.xml :as xml]
            [hiccup2.core :as h]
            [knock.utils :as utils]))


(defn -main [& args]
  (do
    (server/stop-server)
    (server/run-ns {:port 16916}
                   ;(server/namespaces-by "lucy" "main")
                   (server/namespaces-by "lucy" "data")
      ;;...
      ;;
                   )
    ;;
    )
  (println (utils/cur-time-str) " server is running ")
  ;;
  @(promise))


(comment


  )
