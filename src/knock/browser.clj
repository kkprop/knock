
(ns knock.browser
  (:require 
   [etaoin.api :as e]
   [etaoin.keys :as k]
   [clojure.java.io :as io]
   [portal.api :as p]
   [cheshire.core :as json]
   [knock.utils :as u]
   [knock.utils :as utils])
  )

(utils/config :chrome-profile)

(def driver
  (e/chrome {:profile chrome-profile}))

(def click-multi (apply partial e/click-multi [driver]))

(defn go [url]
  (e/go driver url))

(defn click [& q]
  (e/click driver (vec q)))

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
   (map #(e/get-element-text-el driver %)))
  (fun
   (f2
    (f22 ada))
   2 3)

  (def h (take 1000
               (app-history "default" "media" "vos2")))

  (def vd
    (u/map-on-val
     #(map (fn [m] (select-keys m [:idc :operator])) %)
     (group-by #(select-keys % [:version]) h)))

  ;;calc on what version no needed
  (keys vd)

  (go "https://jisho.org/search/慈")

;
  )



(defn list-equal-match [xsa xsb]
  (->>
   (zipmap xsa xsb)
   (filter (fn [[a b]]
             (= a b)))
   (map first)
   ))

(list-equal-match ['driver 'c 'd] ['driver 'c])

(defn gen-single-fn [name entity & to-omit-args]
  (let [m  (meta entity)
        arg-matches (vec (flatten (->> to-omit-args
                                       (map #(list-equal-match (:arglists m) %))
                                       (remove empty?))))]
    (list name arg-matches)))

(defn gen-partial [ns & to-omit-args]
  (->> (ns-publics ns)
       (map #(gen-single-fn (first %) (second %) to-omit-args))
       ))
(gen-partial 'etaoin.api ['driver] ['non])

;;TODO expose all functions in etaoin which start with a driver
(->>
 (ns-publics 'etaoin.api)
 (take 2)
 (map #(meta (second %))))

