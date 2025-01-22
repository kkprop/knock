(ns knock.ticker
  (:require [knock.utils :refer :all]
            [knock.browser :refer :all]
            [knock.tui :as tui]
            [clojure.string :as str]
            [etaoin.api :as e]))

(config
  :ticker-url :config-path "resources/ticker.edn"
  )

(configs [:tg-pre
          :tg-post
          :tl-pre
          :tl-post]
         :config-path "resources/ticker.edn")



(defn ->url [id]
  (let [x (if (str/includes? id "-")
            id
            ;;default us 
            (str id "-" "US"))]

    (when (nil? ticker-url)
      (throw (Exception. "prepare resources/ticker.edn for ticker utilities")))
    (join-path ticker-url x)))

(defn clear-disturb [d]
  (try
    (e/click d {:class "mm-openaccount-close"})
    (e/get-element-inner-html d {:class "en futunn stop-iframe-out-scoll"})
    (catch Exception as e)))

(defn disc-info [d]
  ;;nil if not trading
  (try
    (e/get-element-inner-html d {:class "disc-info"})
    (catch Exception e
      (if (str/includes? (str e)
                         "no such element: Unable to locate element:")
        nil
        ;;unknow exception 
        (throw e)
        )
      ))
  )

(defn price-normal [d]
  (try
    (e/get-element-inner-html d {:class "price-normal"})
    ;(catch Exception e nil ())
    )
  )

(defn cur-price [id]
  (let [d (go (->url id) id)]
    ;;TODO clear by click
    (clear-disturb d)
    (let [pre-post (disc-info d)]
      (if (nil? pre-post)
        (price-normal d)
        pre-post
        ))))


(defn save-tg [s]
  (let [d (join-path "ticker" (cur-date-str))
        f (join-path d (str (cur-ts) "." "edn"))]
    (if (string? s)
      (spit f s)
      (spit-xs f s))))

(def cur-page (atom ""))

(defn cur-volumn [s]
  (if (= (str/includes? s  "nul"))
    0
    (-> (if (digit? (last s))
          s
          (chop-tail-n s 1))
        (precision :keep-digit 3)
        (parse-float))))


(++ cur-volumn volumn)



(defn fix-ticker [m]
  (let [n (if-not (nil? (:code m))
            (rename m :code :ticker)
            m)
        nn
        (if (< 5 (count (:ticker n)))
          (swap-key n :ticker :ticker-name)
          n)]
    (assoc nn
           :idx (force-int (:idx nn))
        )))


(defn locate-cur-tickers []
  (->> (map :text (locate! {:class "table-row table-row-hover"}))
       ;(take-nth 42)
       (map (fn [s]
              (let [xs (str/split-lines s)]
                (when-not (or (= 11 (count xs))
                              (= 12 (count xs)))
                  (println "unknown align" xs))
                (if (= 11 (count xs))
                  (fix-ticker (zipmap [:idx :ticker :ticker-name
                                       :change
                                       :price
                                       :high
                                       :low
                                       :volumn
                                       :range
                                       :pe
                                       :market-cap] xs))
                  (fix-ticker (zipmap [:idx :ticker :ticker-name :code
                                       :change
                                       :price
                                       :high
                                       :low
                                       :volumn
                                       :range
                                       :pe
                                       :market-cap] xs))
                  ;;
                  )))
;
            )
       ;(filter! {:idx "43"})
       ;;
       ))


(defn pre? []
  (or
   (and
    (< 16 (cur-hour))
    (< (cur-hour) 21))
   (and (= (cur-hour) 21)
        (< (cur-min) 30))))

(defn post? []
  (and
   (< 4 (cur-hour))
   (< (cur-hour) 9)))


(defn go-tg []
  (when (mock! pre?)
    (println "go pre ")
    (go tg-pre))
  (when (mock! post?)
    (println "go post ")
    (go tg-post)))

;;find the changing point of pre / post and go to the correct
(defn go-tg! []
  (when
   (< (count (e/get-source (driver)))
      100)
    ;;when found pre
    (when (mock-change? pre?)
      (println "go pre ")
      (go tg-pre))
    ;;when found post
    (when (mock-change? post?)
      (println "go post ")
      (go tg-post))))

(defn watch []
  (let []
    (when (nil? (go-tg) )
      (pause 500)
      )
    (while true
      (let [s (locate-cur-tickers)]
        (println "cur tickers count:" (count s))
        (go-tg)
        ;;only using idx and price. due to market cap and volumn will be changing on market time
        ;(when (apply not= (map #(select-keys % [:idx :price]) [@cur-page s]))
        (when (not= @cur-page s)
          (reset! cur-page s)
          (save-tg s)
          (println "saving")
               ;;
          )
        (println (cur-time-str))
        (if (or (pre?) (post?))
          (pause-minutes 1)
          (pause-minutes 1))
        ;;
        )
      (e/refresh (driver)))
    ;;
    ))

(defn watch! []
  (while true
    (try
      (watch)
      (catch Exception e
        (println (count (str e)))
        (println "rerun")
        (kill-cur-pid-by-name "chromedriver")))))

(defn traject [xs]
  (let [xs (vals (->>
                  (group-by :ticker xs)))]
    ;(println (count xs))
    (->> xs
         (map (fn [xxs]
                (let [x (first xxs)]
                  (assoc x :traj
                         (map :idx xxs))
                  ;;
                  ))))
    ;;
    ))

(defn frame [f]
  (let [ts (force-int (file-name (basename f)))]
    (map #(assoc % :ts ts)
         (slurp-ej-line f))
    )
  )


(defn cur-frame []
  (let [dir (join-path "ticker" (cur-date-str))]
    (frame (last (sort (ls! dir))))
    ;;
    )
  )

(defn cur-frames []
  (let [dir (join-path "ticker" (cur-date-str))]
    (->> (take-last 2 (sort (ls! dir)))
         (map frame)
         )
    ;;
    )
  )

(comment
  (map :ts (cur-frames))
  @cache
  (def cache (atom nil))

  )


(defn track [dir]
  (let []
    (->>
     (sort (ls! dir))
     (mapcat (fn [f]
               (let [ts (force-int (file-name (basename f)))]
                 (map #(assoc % :ts ts)
                      (slurp-ej-line f)))))
     ;(take 200)
     ;(map (fn [xs] (first xs)))
     (map fix-ticker)
     ;(map (juxt :ticker :ts))
     (traject)
     (filter (fn [{:keys [traj]}]
               (in? traj 1)))
     (filter (fn [{:keys [traj]}]
               (= 1 (last traj)))))

;;
    )
;;
  )



(defn compare-frame [[pp cc]]
  (let [p (cur-volumn++ pp)
        c (cur-volumn++ cc)]
    (if (or (nil? pp) (nil? cc))
      (if (nil? pp)
        (assoc (merge pp cc) :speed 666)
        (assoc (merge pp cc) :speed -1))
      (assoc c :speed (- (:cur-volumn c) (:cur-volumn p)))
;;
      )
    ;;
    ))

(defn live []
  (let [cache (atom {})
        p (promise)]

    (thread!
     (while true
       (let [[prev cur] (cur-frames)]
         (when-not (empty? cur)
               ;; compare
           (let [xs
                 (reverse (sort-by :speed (-> (map-on-val compare-frame (group-by :ticker (concat prev cur)))
                                              vals
                                              flatten)))]
             (println (apply str (repeat 80 "-")))
             (map!! println
              (str/split-lines (pp-hashmap @cache :ticker :speed :volunm :change :market-cap :price)))
             (reset! cache xs)))
         (pause 10000)
           ;;
         )))

    (thread!
     (while true
       (let [iid (atom "")
             cur-task (atom nil)]
         (while (not (realized? p))
           ;; do a new rendering 
           (when-not (= @iid (->uuid @cache))
             (println (apply str (repeat 80 "-")))
             ;;stop previous
             (when-not (nil? @cur-task)
               (.interrupt @cur-task))
             (let [t (tui/render
                      (str/split-lines (pp-hashmap @cache :ticker :speed :volunm :change :market-cap :price))
                      (fn [x]
                        (println "user choose " x)))]
               (reset! cur-task t)
               (reset! iid (->uuid @cache))))

           (Thread/sleep 1000)

         ;;
           ))))
    @p
;;
    ))



(comment

  (def cache (atom {}))
  (reset! cache prev

  (-> (:volumn
       (def m (first cur))))

  (cur-volumn (:volumn (last prev)))

  (pp
   (map-on-val compare-frame
               (group-by :ticker (concat prev cur))
               ;;
               ))

  (def f "/home/devops/knock/ticker/2025-01-18/1737147664.edn")
  (->abs-path "~/kkprop/knock/ticker/table.edn")

  (nth
   (map :text (locate! {:class "table-row table-row-hover"}))
   42)

;;
  ) 


)
