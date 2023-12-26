(ns knock.browser
  (:require
   [etaoin.api :as e]
   [etaoin.keys :as k]
   [clojure.java.io :as io]
   ;[portal.api :as p]
   [taoensso.timbre :as timbre]
   [cheshire.core :as json]
   [clojure.edn :as edn]
   [clojure.data.xml :as xml]
   [knock.utils :as utils :refer :all]
   [clojure.pprint :as pp]
   [clojure.string :as str]))

;; disable etaoin detail log
(timbre/set-level! :info)

(defn- timeout [timeout-ms callback]
  (let [fut (future (callback))
        ret (deref fut timeout-ms :timed-out)]
    (when (= ret :timed-out)
      (future-cancel fut))
    ret))

(defn conn [driver]
  (utils/select driver :host :port))

(def driver-edn-path "etaoin-driver-conn.edn")

(utils/config :chrome-profile)

(defn new-driver []
  (let [d
        (e/chrome
          {:profile chrome-profile}
         )
        c (conn d)]
    (spit driver-edn-path (with-out-str (clojure.pprint/pprint c)))
    d)
  )

(defn load-driver []
  (let [d
        (try (timeout 1000 (partial
                            e/chrome
                            (assoc (utils/load-edn driver-edn-path)
                                   :headless :true)))
             ;;any exception happen means we need a new-driver too.
             ;;  just return :timed-out to trigger a new-driver call
             (catch Exception e :timed-out))]
    (if (= d :timed-out)
      (try
        (new-driver)
        (catch Exception e
          (run-cmd "open" "https://googlechromelabs.github.io/chrome-for-testing/#stable")))
      d)
    ;;
    ))

(def driver
  (load-driver)
  )


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

(defn locate [url q uniq-text]
  (go url)
  (e/wait-visible driver q)
  (->> (e/query-tree driver q)
       (map (fn [id] {:id id :text (e/get-element-text-el driver id)}))
       (filter #(utils/fuzzy-rev-in? [uniq-text] (:text %)))
       (map #(assoc % :html (e/get-element-inner-html-el driver (:id %))))
       ;;
       )
  )

(comment


  (flatten-hashmap
   (->>
    (:content
     (xml/parse-str
      (str "<tbody>"
           (trim-to
            (:html (first media))
            "<tr"))))

    (take 6))
   ;;
   )

  (locate "https://clojure.org/news/2022/03/20/deref"
          {:class "sect1"}
          "Blogs")

  (wiki "Philip H. Dybvig")
  (->>
   (e/query-tree driver :content-root {:tag :article})
   (map #(e/get-element-text-el driver %)))
  (fun
   (f2
    (f22 ada))
   2 3)

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

