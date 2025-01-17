(ns knock.ticker
  (:require [knock.utils :refer :all]
            [knock.browser :refer :all]
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
    (go tg-pre))
  (when (mock! post?)
    (go tg-post)))


;;find the changing point of pre / post and go to the correct
(defn go-tg! []
  (when
   (< (count (e/get-source (driver)))
      100)
    ;;when found pre
    (when (mock-change? pre?)
      (go tg-pre))
    ;;when found post
    (when (mock-change? post?)
      (go tg-post))))

(defn watch []
  (let []
    (go-tg)
    (while true
      (e/refresh (driver))
      (let [s (locate-cur-tickers)]
        (go-tg!)
        (when (not= @cur-page s)
          (reset! cur-page s)
          (save-tg s)
          (println "saving")
               ;;
          )
        (println (cur-time-str))
        (if (or (pre?)
                (post?))
          (pause-minutes (+ 1 (* 0.1 (rand-int 3))))
          (pause-minutes (+ 1 (* 0.1 (rand-int 3)))))
        ;;
        ))
;;
    ))

(defn watch! []
  (while true
    (try (watch)
         (catch Exception e
           (println e)
           (println "rerun")
           )
         )

    )
  )

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
               (in? traj 1)
               ))
     (filter (fn [{:keys [traj]}]
               (= 1 (last traj))
               ))
     pp
     )
    ;;
    )
;;
  )


(comment
  (->abs-path "~/kkprop/knock/ticker/table.edn")

  (nth
   (map :text (locate! {:class "table-row table-row-hover"}))
   42)

;;
  )
