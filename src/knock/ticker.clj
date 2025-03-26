(ns knock.ticker
  (:require [knock.utils :refer :all]
            [knock.browser :refer :all]
            [knock.tui :as tui]
            [clojure.string :as str]
            [etaoin.api :as e]
            [clojure.walk :as cljwalk]))

(config
  :ticker-url :config-path "resources/ticker.edn"
  )

(configs [:tg-pre
          :tg-post
          :tl-pre
          :api-key
          :tl-post
          ]
         :config-path "resources/ticker.edn")

(def url "https://www.alphavantage.co/")

(defn vg-keyword [x]
  (if (keyword? x)
    (->keyword
     (let [s (str/lower-case (->str x))]
       (if (nil? (->int s))
         s
         (chop-leading-n s 4))))
    x))

(defn query [f & kv]
  (let [{:keys [body err]
         :as m} (curl-get
                 (apply make-url url :query :function f :apikey api-key kv))]
    (when-not (nil? err)
      (println err))
    (clojure.walk/postwalk vg-keyword body)))


(defn symbol [id]
  (query :GLOBAL_QUOTE :symbol id)
  )

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
    (locate! {:class "disc-info"})
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
    (locate! {:class "price-normal"})
    ;(catch Exception e nil ())
    )
  )

(defn cur-price [id]
  (let [d (go (->url id) id)]
    ;;TODO clear by click
    ;(clear-disturb d)
    (locate! {:class "price-normal"})))

(comment

  (def id  "CNEY")
  (cur-price id)

  )


(defn save-tg [s]
  (let [d (join-path "ticker" (cur-date-str))
        f (join-path d (str (cur-ts) "." "edn"))]
    (if (string? s)
      (spit f s)
      (spit-xs f s))))

(def cur-page (atom ""))

(defn mk->k [s]
  (if (empty? s)
    0
    (let [unit (last s)
          value (-> (chop-tail-n s 1)
                    (precision :keep-digit 3)
                    parse-float)]
      (case unit
        \M (* value 1000)
        \K value
        (/ value 1000))))

;;
  )

;(defn cur-volumn [s]
;  (if (or (str/includes? s  "nul")
;          (empty? s)
;          )
;    0
;    (mk->k s)
;    )
;  )

(defn cur-volumn [s]
  (if (str/includes? s  "nul")
    0
        (-> (if (digit? (last s))
          s
          (chop-tail-n s 1))
        (precision :keep-digit 3)
        (parse-float))
        ))

(++ cur-volumn volumn)

;(def s (:volumn prev))


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
    (< 15 (cur-hour))
    (< (cur-hour) 21))
   (and (= (cur-hour) 20)
        (< (cur-min) 30))))

(defn post? []
  (and
   (< 3 (cur-hour))
   (< (cur-hour) 8)))


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


(defn cur-all-frame []
  (let [dir (join-path "ticker" (cur-date-str))]
    (map frame (sort (ls! dir)))
    ;;
    ))


(defn cur-frame []
  (let [dir (join-path "ticker" (cur-date-str))]
    (frame (last (sort (ls! dir))))
    ;;
    )
  )

(defn cur-frames
  []
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




(defn compare-frame [[prev cur]]
  (let []
    (if (or (nil? prev) (nil? cur))
      (if (nil? prev)
        (assoc (merge prev cur) :speed 666)
        (assoc (merge prev cur) :speed -1))
      (let [p (cur-volumn++ prev)
            c (cur-volumn++ cur)]
        (assoc c :speed
               (let [speed  (- (:cur-volumn c) (:cur-volumn p))]
                 (if (str/ends-with? (:volumn c) "M")
                   (* 1000 speed)
                   (if (str/ends-with? (:volumn c) "K")
                     speed
                     (/ speed 1000.0))
                   )
                 )
               )
;;
      ))
    ;;
    ))


(def cols-ticker [:ticker :speed :volumn :idx :change :market-cap :price])
(defn live []
  (let [cache (atom {})
        p (promise)]

    (thread!
     ;;( def cache {})
     (while true
       (let [xs (cur-frames)
             prev (first xs)
             cur (second xs)]
         (when-not (empty? cur)
           ;; compare
           (let [xs (reverse (sort-by :speed (-> (map-on-val compare-frame (group-by :ticker (concat prev cur)))
                                                 vals
                                                 flatten)))]
             (when-not (= (->uuid xs) (->uuid @cache))
               ;(println xs)
               (println (apply str (repeat 80 "-"))
                        "lag: " (- (cur-ts) (:ts (first xs))) "s"
                        (cur-time-str))
               (map!! println
                      (str/split-lines (apply pp-hashmap
                                                                                            ;;
                                              (->> xs
                                                   (map #(assoc % :speed (precision (str (:speed %)))))
                                                   ;;afterwards processing
                                                   (map (fn [{:keys [ticker]
                                                              :as m}]
                                                          (if (in? (keys (group-by :ticker @cache))
                                                                   ticker)
                                                            m
                                                            (assoc m :ticker (str "**" ticker "**"))))))

                                              cols-ticker)))
               (reset! cache xs))))
         (pause 10000)
           ;;
         )))

    ;(thread!
    ; (while true
    ;   (let [iid (atom "")
    ;         cur-task (atom nil)]
    ;     (while (not (realized? p))
    ;       ;; do a new rendering 
    ;       (when-not (= @iid (->uuid @cache))
    ;         (println (apply str (repeat 80 "-")))
    ;         ;;stop previous
    ;         (when-not (nil? @cur-task)
    ;           (.interrupt @cur-task))
    ;         (let [t (tui/render
    ;                  (str/split-lines (apply pp-hashmap @cache cols-ticker))
    ;                  (fn [x]
    ;                    (println "user choose " x)))]
    ;           (reset! cur-task t)
    ;           (reset! iid (->uuid @cache))))

    ;       (Thread/sleep 1000)

    ;     ;;
    ;       ))))
    @p
;;
    ))


(defn taliyun [& args]
  (apply run-cmd "aliyun --config-path ~/.aliyun/tconfig.json" args)
  )
(defn tstart []
  (pp (taliyun "ecs StartInstance --InstanceId i-j6cjbk2s5jdkc5voxym4")))

(defn tstop []
  (pp (taliyun "ecs StopInstance --InstanceId i-j6cjbk2s5jdkc5voxym4")))

(defn auto-tstop []

  (cur-week-day))

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

                                        ;
          ))

(defn start-collect []
  (while true
    (if (et-true pre?)
      (do
        (println "start machine"
                 (tstart))
        (pause-seconds 32)
        (println "start service" (run-cmd "~/ss/ticker.sh sudo service collect restart")))
      (do (pause-seconds 3)
          (println "waiting pre")))))


(defn track-ticker []
  (count
   (take-last 10 (mock cur-all-frame))
   )
  )


