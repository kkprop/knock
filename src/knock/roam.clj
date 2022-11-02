(ns knock.roam
  (:require
   [babashka.curl :as curl]
   [clojure.string :as str]
   [cheshire.core :as json]
   [knock.utils :as utils :refer :all]
   ))

(def g {:name "xzl"
        :token "roam-graph-token-dW9r1QFGTupMhchw-dmp4mC1Jdw0w4I77pRCVlp6"})

(def base "https://api.roamresearch.com/api/graph")


(defn join-url [ & xs]
  (str/join "/" xs))

;;assemble url that Roam API needed
(defn api-url [g op]
  (join-url base (:name g) op))

;;uid for today's daily note
(defn cur-daily-page []
  (cur-time-str "MM-dd-yyyy"))


(defn cur-daily-page-title []
  (cur-time-str "MMMM dd, yyyy")
  )
;;
(defn post [g route data]
  (let [url (api-url g route)
        token (:token g)
        headers  {"Content-Type" "application/json"
                  "accept" "application/json"
                  "Authorization" (str "Bearer " token)
                  }
        req {:headers  headers
             :body (json/generate-string data)
             :raw-args ["--location-trusted"]
             }
        ]
    (json/parse-string
     (:body
      (curl/post url req)))
    )
  )

(declare create-page update-page update-block  move-block)

;;create block, default in today's daily notes
(defn write [g string & {:keys [page order open heading text-align]
                       :or {page (cur-daily-page) order "last" open false heading 3 text-align "right" children-view-type "document"}
                       :as opt
                       }]
  (let [data {
              :action "create-block"
              :location {:parent-uid page
                         :order order 
                         }
              :block {:string string
                      :open open
                      ;:heading heading
                      :text-align text-align
                      }
              }
        ]
    (try 
      (post g "write" data)
      (catch Exception e
        (let [msg (.getMessage e)]
          (if (= msg "babashka.curl: status 400")
            ;;probably no daily note try create
            (do 
              (create-page g {:title "October 31st, 2022" :uid (cur-daily-page)})
              (post g "write" data)
              )
            msg)
          )
        )
      )
    ))

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
  (let [data {:eid (str (eval eid))
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
        (clojure.string/replace 
         (with-out-str
           (clojure.pprint/pprint code)
           )
         #"clojure.core/"
         ""
         )
        )
  )

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

(comment
  (macroexpand 
   (gen-json-fn
    (load-json "write.json")
   )

  (update-block g {:uid "w-9UmjI-0" :string "Hello Roam!"} )
  (def gt
    g
    ;(load-edn "Tickers.edn")
    ;(load-edn "/Users/dc/dc.edn")
    )
  (write gt "Hello" {:heading 3 :text-align "left"})

  (cur-daily-page)

  (q gt '[:find ?block-uid ?block-str
               :in $ ?search-string ?search-string2
               :where [?b :block/uid ?block-uid]
               [?b :block/string ?block-str]
               [(clojure.string/includes? ?block-str ?search-string)]
               [(clojure.string/includes? ?block-str ?search-string2)]
               ]
     "Roam " "API"
     )

  (pull dc '[:block/uid (cur-daily-page)]
        '[:block/uid :node/title :block/string
          {:block/children [:block/uid :block/string]}
          {:block/refs [:node/title :block/string :block/uid]}
          ]
        )

  (q dc
     ')

  (def d (load-json "roam-api.json"))

  (def j {"update-page"
          {"action" "update-page"
           "page" {"uid" "xK98D8L7U",
                   "title" "List of participants"}}}
    )
  (def fname "update-page")
  (def ks ['action 'uid 'title])
  (def defaults ['action "update-page" 'uid "xK98D8L7U" 'title "List of participants"] )
  (fn-parts "update-page" j)

  ))

