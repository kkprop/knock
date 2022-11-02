(ns knock.browser
  (:require 
   [etaoin.api :as e]
   [etaoin.keys :as k]
   [clojure.java.io :as io]
   [portal.api :as p]
   [cheshire.core :as json]
   [knock.utils :as u]
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


(defn kv-url [& args]
  ;app=%s&&project=media&&env=default"
  (let [xs (partition 2 args) 
        cols (map #(format "%s=%s" (name (first %)) (second %)) xs)]
    (clojure.string/join "&&" cols)
    )
  )

(defn app-history [env project app]
  (let [url (str "https://deploy.agoralab.co/api/v1/deploy/action/history?"
                 (kv-url
                  :env env
                  :project project
                  :app app
                  ))]
     (go url)
     (e/wait-has-text-everywhere driver "data")
     (:data (:data (json/parse-string
                    (e/get-element-text driver {:style "word-wrap: break-word; white-space: pre-wrap;"})
                    true
                    )))
    )
   )


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

  (def h (take 1000 
               (app-history "default" "media" "vos2" )))

  (def vd
    (u/map-on-val
     #(map (fn [m] (select-keys m [:idc :operator])) %)
     (group-by #(select-keys % [:version]) h)))

  ;;calc on what version no needed
  (keys vd)

;
  )
