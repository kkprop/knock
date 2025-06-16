
(ns knock.roam
  (:require
   [babashka.curl :as curl]
   [clojure.string :as str]
   [cheshire.core :as json]
   [knock.server :as server :refer :all]
   [knock.utils :as utils :refer :all]
   ))

;;invalid
(def g {:name "xzl"
        :token "roam-graph-token-dW9r1QFGTupMhchw-dmp4mC1Jdw0w4I77pRCVlp6"})

(defn xzl []
  (load-edn "/Users/dc/xzl.edn")
  )

(defn personal []
  (load-edn (->abs-path "~/roam.edn")))


(def base "https://api.roamresearch.com/api/graph")


(defn join-url [ & xs]
  (str/join "/" xs))

;;assemble url that Roam API needed
(defn api-url [g op] (join-url base (:name g) op))

;;uid for today's daily note
(defn cur-daily-page []
  (cur-time-str "MM-dd-yyyy"))

(defn cur-daily-page-title []
  (let [num-suffix {"1" "st" "2" "nd" "3" "rd" "21" "st" "22" "nd" "23" "rd" "31" "st"}
        [m d y] (cur-time-str "MMMM " "d" ", yyyy")
        ]
        (str m (str d (get num-suffix d "th")) y)
    )
    )
;;
(defn post [g route data]
  (let [url (api-url g route)
        token (:token g)
        headers  {"Content-Type" "application/json"
                  "accept" "application/json"
                  "Authorization" (str "Bearer " token)}
        req {:headers  headers
             :body (json/generate-string data)
             :raw-args ["--location-trusted"]}]

    (:body (curl-post url req)))
  )

(declare create-page update-page update-block  move-block)

;;create block, default in today's daily notes
(defn write [g string & {:keys [page order open heading text-align]
                         :or {page (cur-daily-page) order "last" open false heading 3 text-align "left" children-view-type "document"}
                         :as opt}]
  (let [data {:action "create-block"
              :location {:parent-uid page
                         :order (if-nil-then order "last")}
              :block {:string string
                      :open open
                      ;:heading heading
                      :text-align (if-nil-then text-align "left")}}]

    (try
      (println "writing" data)
      (post g "write" data)
      (catch Exception e
        (let [msg (.getMessage e)]
          (if (= msg "babashka.curl: status 400")
            ;;probably no daily note try create
            (do
              (create-page g {:title (cur-daily-page-title) :uid (cur-daily-page)})
              (post g "write" data)
              (println data))
            msg))))))

(defn q [g query & args]
  (let [data {
              :query (str query)
              :args args
              }
        ]
    (
     post g "q" data)
    )
  )

(defn pull [g eid selector]
  (let [data {:eid (str eid)
              :selector (str selector)
              }]
    (post g "pull" data)
    )
  )


;;TODO
;;"batch-actions": {"action": "batch-actions", "actions": []},
;;

(defn create-page [g {:keys [action uid title children-view-type],
                      :or {uid "", title "List of participants", children-view-type "document",
                                                                        action "create-page"},
                      :as opt}]
  (let
      [url
       (api-url g "write")
       data
       {"action" action,
        "page"
        {"uid" uid,
         "title" title,
         "children-view-type" children-view-type}}]
    (post g "write" data)))
(defn update-page [g {:keys [action uid title], :or
                      {uid "xK98D8L7U",
                       title "List of participants (updated)",
                       action "update-page"},
                      :as opt}]
  (let [url (api-url g "write")
        data {"action" action, "page" {"uid" uid, "title" title}}]
    (post g "write" data)))
(defn update-block [g {:keys [action uid string open heading text-align children-view-type],
                       :or
                       {heading 2,
                        open false,
                        text-align "center",
                        uid "51v-orCLm",
                        string "new string from the backend",
                        children-view-type "document",
                        action "update-block"},
                       :as opt}]
  (let
      [url (api-url g "write")
       data {"action" action,
             "block"
             {"uid" uid,
              "string" string,
              "open" open,
              "heading" heading,
              "text-align" text-align,
              "children-view-type" children-view-type}}]
    (post g "write" data)))
(defn move-block [g {:keys [action uid parent-uid order],
                     :or
                     {parent-uid "09-27-2022",
                      uid "7yYBPW-WO",
                      order 3,
                      action "move-block"},
                     :as opt}]
  (let
      [url
       (api-url g "write")
       data
       {"action" action,
        "block" {"uid" uid},
        "location" {"parent-uid" parent-uid, "order" order}}]
    (post g "write" data)))

(defn fn-parts [name data]
  (let [fname (symbol name)
        args (->> (flatten-hashmap data) 
                  (map #(map-on-key last %)))
        ks (map read-string (flatten (map keys args)))
        defaults (flatten
                  (into [] cat
                        (map #(map-on-key read-string %) args)))
        json-symbols (reduce (fn [a c]
                               (let [[k v] (first c)]
                                    (update-in a k
                                               (fn [v] (symbol (last k)))
                                               )
                                    )
                               ) {} (flatten-hashmap data)
                           )
                 ]
    (list fname ks defaults json-symbols)
         ))


(defmacro replace-all [s & args]
  `(-> s
       ~@(for [k (eval args)]
          (clojure.string/replace ~(re-pattern k) "")
          )
      )
  )

(defn append-code [file-name code]
  (spit file-name 
        (clojure.string/replace (with-out-str
           (clojure.pprint/pprint code)
           )
         #"clojure.core/"
         ""
)))

(defmacro gen-json-fn [j file-name]
  `(do
     ~@(for [f (keys (eval j))
             :let [d (get (eval j) f)
                   [fname ks defaults json-symbols] (fn-parts f d)
                   ]
             ]
         (append-code file-name 
                      `(defn ~fname [g {:keys ~(apply vector ks)
                                        :or ~(apply hash-map defaults)
                                        :as opt}]
                         (let [url (api-url g "write")
                               ;;TODO
                               data ~json-symbols
                               ]
                           (post g "write" data)
                           )
                         ))
         )))

;;generate other write function
;;previously declared
;(gen-json-fn
; (load-json "write.json")
; "src/knock/roam.clj"
; )

(defn search-block [g s]
  (let [res (q g '[:find ?block-uid ?block-str
                   :in $ ?search-string
                   :where [?b :block/uid ?block-uid]
                   [?b :block/string ?block-str]
                   [(clojure.string/includes? ?block-str ?search-string)]]
               s)]
    (name-xs [:block/uid :block/string]  (:result res))
    ;;
    ))

(defn pull-uid [g uid]
  (let []
    (:result (pull g (format "[:block/uid \"%s\"]" uid)
                   '[:block/uid :node/title :block/string
                     {:block/children [:block/uid :block/string]}
                     {:block/refs [:node/title :block/string :block/uid]}]))))

(defn pull-daily-note [g]
  (pull-uid g (cur-daily-page))
  )

(defn daily-note-block
  ([g]
   (let [uid (cur-daily-page)]
     (if-nil-then (->>
                   (:block/children (pull-uid g uid))
                   (filter! {:block/string "[[一天]]"})
                   first
                   :block/uid
      ;;
                   )
                  uid)))
  ;;search child blocks for first block in daily note
  ([g s]
   (let [uid (daily-note-block g)]
     uid
     (if-nil-then
      (->>
       (:block/children (pull-uid g uid))
       (filter (fn [m] (str/includes? (:block/string m) s)))
       first
       :block/uid)
      (do
        (go!
          (pause-seconds 30)
          (mock! daily-note-block g s)
          )
        uid)))))

(defn personal-block [g]
  ;;(def g (personal))
  ;;(def s "#Personal")
  (let [x (mock-within 1800 daily-note-block g "#Personal")]
    ;(pull-daily-note g x)
    ;(when-not (empty? (re-seq #"\d\d-\d\d-\d\d\d\d" "21-05-2025")) (mock! daily-note-block g "#Personal"))
    x))

(defn work-block [g]
  (mock-within 1800 daily-note-block g "#Work"))

(.local-load)
;;make sure
(.local :roam/prev)

(defn pb->roam []
  (let [x (if-nil-then (.slurp :pb) "")
        ;;doing decoration
        s (cond
            (str/includes? x "::") (str "`" x "`")
            (str/includes? x "摘录来自") (epub-clean x)
            :else x)
        prev (if-nil-then (.slurp :roam/prev) [])
        work-uid (work-block (personal))
        personal-uid (personal-block (personal))]

    (when (and (not (empty? s))
               (not (str/includes? s "-----BEGIN CERTIFICATE-----"))
               (not (str/includes? s "-----BEGIN CERTIFICATE REQUEST-----"))
               (not (str/includes? s "-----BEGIN RSA PRIVATE KEY-----"))
               (not (in? prev s))
               (not (digit? s))
               (not (in? ["Roam Research"] (front-most-app))
               ;;
                    ))
      (println (front-most-app))
      (.cons-cap 101 :roam/prev s)
      ;(def s "148-153-61-217")
      (if (ip? (->ip s))
        (write (personal) s :page work-uid :order "first")
        (write (personal) s :page personal-uid)))

;;;
    ))

(defn run-pb->roam []
  (let [p (watch-pb pb->roam)]
    ;;
    @p
    )
  )
;;

(comment
  (str/includes? "::abc" "::")

  (pbpaste)

  (go!
   (run-pb->roam))

  (pbpaste)
  (server/local-call :rand)
  (macroexpand
   (gen-json-fn
    (load-json "write.json"))

   (update-block g {:uid "w-9UmjI-0" :string "Hello Roam!"})
   (def gt
    ;g
    ;(load-edn "Tickers.edn")
     )
   (write gt "Hello" {:heading 3 :text-align "left"})

   (cur-daily-page)
   (write gt "Hello")

   (q gt '[:find ?block-uid ?block-str
           :in $ ?search-string ?search-string2
           :where [?b :block/uid ?block-uid]
           [?b :block/string ?block-str]
           [(clojure.string/includes? ?block-str ?search-string)]
           [(clojure.string/includes? ?block-str ?search-string2)]]
      "library" "zlib")
   (def d (load-json "roam-api.json"))

   (def j {"update-page"
           {"action" "update-page"
            "page" {"uid" "xK98D8L7U",
                    "title" "List of participants"}}})
   (def fname "update-page")
   (def ks ['action 'uid 'title])
   (def defaults ['action "update-page" 'uid "xK98D8L7U" 'title "List of participants"])
   (fn-parts "update-page" j))

  (->>
   (search-block (xzl)
                 "三月")
   (map :block/uid))

  (pp (pull-uid (personal)  "TnKJ1RiBH"))
  (pp (pull-uid (personal) "jUaOmeNk7"))

;;
  )

