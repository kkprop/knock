(ns knock.ticker-replay-main
  (:require [knock.ticker-replay :as replay]))

(defn -main [& args]
  (apply replay/-main args))
