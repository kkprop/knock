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


(defn join-url [& xs]
  (str/join "/" xs))

;;assemble url that Roam API needed
(defn api-url [g op]
  (join-url base (:name g) op))

;;uid for today's daily note
(defn cur-daily-page []
  (cur-time-str "MM-dd-yyyy"))

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

;;create block, default in today's daily notes
(defn write [g text & {:keys [page order open heading text-align]
                       :or {page (cur-daily-page) order "last" open false heading 3 text-align "right" children-view-type "document"}
                       :as opt
                       }]
  (let [data {
              :action "create-block"
              :location {:parent-uid page
                         :order order 
                         }
              :block {:string text
                      :open open
                      :heading heading
                      :text-align text-align
                      }
              }
        ]
    (post g "write" data)
    ))

(defn q [g query & args]
  (let [data {
              :query (str query)
              :args args
              }
        ]
    (post g "q" data)
    )
  )

(defn pull [g eid selector]
  (let [data {:eid (str eid)
              :selector (str selector)
              }]
    (post g "pull" data)
    )
  )


(defn fn-parts [name data]
  (let [fname (symbol name)
        args (->> (flatten-hashmap data) 
                  (map #(map-on-key last %)))
        ks (map read-string (flatten (map keys args)))
        defaults (flatten
                  (into [] cat
                        (map #(map-on-key read-string %) args)))
                 ]
    (list fname ks defaults)
         ))

(defmacro update-json [j]
  (let [m (eval j)
        xs (flatten-hashmap m)
        ]
     m
    )
  )

(defmacro gen-json-fn [j]
  `(do
     ~@(for [f (keys (eval j))
             :let [d (get (eval j) f)
                   [fname ks defaults] (fn-parts f d)
                   ]
             ]
         `(defn ~fname [g {:keys ~(apply vector ks)
                         :or ~(apply vector defaults)
                         :as opt}]
            (let [url (api-url g "write")
                    ;;TODO
                    ;data (merge j) 
                    ]
                )
            )
         )))

(comment
  (macroexpand 
   (gen-json-fn
    (load-json "write.json")
    )
   )

  (write g
   "Hello Roam!")

  (cur-daily-page)

  (q g '[:find ?block-uid ?block-str
              :in $ ?search-string ?search-string2
              :where [?b :block/uid ?block-uid]
              [?b :block/string ?block-str]
              [(clojure.string/includes? ?block-str ?search-string)]
         [(clojure.string/includes? ?block-str ?search-string2)]
         ]
     "Roam" "API"
     )

  (pull g '[:block/uid "10-28-2022"]
        '[:block/uid :node/title :block/string
          {:block/children [:block/uid :block/string]}
          {:block/refs [:node/title :block/string :block/uid]}]
        )

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

  )
