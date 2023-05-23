(ns lucy.out.main
  (:gen-class)
  (:require [lucy.server :refer [run] :as server]
            [lucy.core :as core]
            [clojure.core.async :as a]
            [clojure.string :as str]))

(defn -main [& args]
  (do
    (server/stop-server)
    (run {:port 16261}
         (server/namespaces-by "lucy" "main")
      ;;...
      ;;
     )
    ;;
    ))
