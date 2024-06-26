(ns knock.browser
  "
  [[new-driver]]  
  [[conn]]
  [[load-driver]]
  "
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
   (utils/select driver :host :port)
   )

(def driver-edn-path "etaoin-driver-conn.edn")

(utils/config :chrome-profile)

;;load a new etaoin driver load one
(defn new-driver []
  (let [d (e/chrome
          {:profile chrome-profile})
        c (conn d)]
    (spit driver-edn-path (with-out-str (clojure.pprint/pprint c)))
    d
    ))

;;using existing driver, load a new session
(defn load-driver []
  (let [d (try (timeout 3000 (partial e/chrome (utils/load-edn driver-edn-path)))
             ;;3 seconds should be loaded
             ;;any exception happen means we need a new-driver too.
             ;;  just return :timed-out to trigger a new-driver call
             (catch Exception e :timed-out))]
    (if (= d :timed-out)
      (try
        ;;try load a new chrome window
        (new-driver)
        (catch Exception e (run-cmd "open" "https://googlechromelabs.github.io/chrome-for-testing/#stable")))
      d)
    ;;
    ))

;;a load function load the instance
;;cache the instance in the variable
;; var-name f
;;
(def driver-cache (atom {}))

(defn driver []
  (let []
    (when (empty? @driver-cache)
      (reset! driver-cache (load-driver))
      )
    (try
      (e/get-source @driver-cache)
      (catch Exception e
        ;(let [res (ex-data e)])
        ;not valid just load again
        (println e "not ok, load a new-driver")
        (reset! driver-cache (load-driver))
        )
      )
    @driver-cache
    ))

(comment
  (driver)

  (e/with-chrome driver
    (e/go driver "https://clojure.org"))

  (locate "https://www.futunn.com/heatmap-us"
          (driver)
          "stock-treemap")

  (println
   (e/get-element-inner-html
    (driver) {:class "stock-treemap"}))

  (reset! driver-cache {})
  (go "https://www.163.com")
  (go "https://zh.wikisource.org")
  (e/get-status (driver))
  (e/delete-session (driver)))


(defn go [url]
  (e/go (driver) url)
  )

(defn click [& q]
  (e/click (driver) (vec q))
  )

(defn search [url word]
  (e/go (driver) (str url word)))

(defn wiki [word]
  (e/with-chrome driver
    (e/go driver (str "https://www.wikiwand.com/en/" word))
    )
  )


(comment
  (driver)
  (new-driver)
  (wiki-source "信心铭")
  )

(defn kv-url [& args]
  ;app=%s&&project=media&&env=default"
  (let [xs (partition 2 args) 
        cols (map #(format "%s=%s" (name (first %)) (second %)) xs)]
    (clojure.string/join "&&" cols)
    )
  )

(defn locate!! [url q uniq-text]
  (go url)
  (e/wait-visible (driver) q)
  (->> (e/query-tree (driver) q)
       (map (fn [id] {:id id :text (e/get-element-text-el (driver) id)}))
       (filter #(utils/fuzzy-rev-in? [uniq-text] (:text %)))
       (map #(assoc % :html (e/get-element-inner-html-el (driver) (:id %))))
       ;;
       ))



(defn locate! [q uniq-text]
  (let [d (driver)]
    (->> (e/query-tree d q)
         (map (fn [id] {:id id :text (e/get-element-text-el d id)}))
         (filter #(utils/fuzzy-rev-in? [uniq-text] (:text %)))
         (map #(assoc % :html (e/get-element-inner-html-el d (:id %))))
         (map #(assoc % :tag (e/get-element-tag-el d (:id %))))
         ;;
         )))

(defn has-text? [uniq-text {:keys [text]}]
  (utils/fuzzy-rev-in? [uniq-text] text)
  )

(defn el-children [driver el q uniq-text]
  (let []
    (->>
      (e/children driver el q)
      (map (fn [id] {:id id :text (e/get-element-text-el driver id)}))
      (filter (partial has-text? uniq-text)
      )
    )
  )) 

(defn el-children! [el q uniq-text]
  (el-children (driver) el q uniq-text)
  )

(defn locate-in! [q uniq-text]
  (let [d (driver)]
    (->> (e/query-tree d q)
         (map (fn [id] {:id id :text (e/get-element-text-el d id)}))
         (filter #(in? [uniq-text] (:text %)))
         (map #(assoc % :html (e/get-element-inner-html-el d (:id %))))
         (map #(assoc % :tag (e/get-element-tag-el d (:id %))))
         ;;
         )))

(defn ->el [x]
  (:id (first x))
  )

(defn click-el! [id]
  (e/click-el (driver) id)
  )

(comment

  (driver)



  (e/get-source
   (driver)
   )
  (go "https://www.google.com")
  (e/get-source (driver))

  (defn ->opts [{:keys [host port] :as m}]
    (assoc m
           :url "https://www.google.com"))

  (def dd
    (e/chrome
     (->opts
      (conn (driver))))
    )

  (driver)
  dd

  (e/go dd "https://www.google.com")

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
   (e/query-tree (driver) :content-root {:tag :article})
   (map #(e/get-element-text-el (driver) %)))
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
;; alternative just using (driver)
(comment
  (go "https://waytoagi.feishu.cn/wiki/QPe5w5g7UisbEkkow8XcDmOpn8e")

  (map :text
       (locate! {:class "text"} ""))
  (def catalogue (locate! {:class "tree-title-content-title ellipsis"} ""))
  (map :text catalogue)
  (click-el! (:id (second catalogue)))

  (->>
   (ns-publics 'etaoin.api)
   (take 2)
   (map #(meta (second %)))))
