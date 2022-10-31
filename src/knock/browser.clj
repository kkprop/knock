(ns knock.browser
  (:require 
   [etaoin.api :as e]
   [etaoin.keys :as k]
   [clojure.java.io :as io]
   [portal.api :as p]
   )
  )


(def chrome-profile
  "/Users/dc/Library/Application Support/Google/Chrome/Profile 2/Confluence")

(def driver (e/chrome {:profile chrome-profile}))

(defn go [url]
  (e/go driver url))


(defn search [url word]
  (e/go driver (str url word) )
  )

(defn wiki [word]
  (search "https://www.wikiwand.com/en/" word))



(comment
  (wiki "Philip H. Dybvig")
  (->> 
   (e/query-tree driver :content-root {:tag :article})
   (map #(e/get-element-text-el driver %))
   )
  (fun
   (f2
    (f22 ada))
   2 3 )
         )
