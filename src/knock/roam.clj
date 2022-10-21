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
  (str/join "/" xs)
  )
(defn api-url [g op]
  (join-url base (:name g) op)
  )

(defn cur-daily-page []
  (cur-time-str "MM-dd-yyyy")
  )

(defn pull [g ]
  )

(defn gen-json-fn []

  )

(defn write [g text & {:keys [page order open heading text-align]
                       :or {page (cur-daily-page) order "last" open false heading 3 text-align "right" children-view-type "document"}
                       :as opt
                       }]
  (let [url (api-url g "write")
        data {
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
        headers  {"Content-Type" "application/json"
                  "accept" "application/json"
                  "Authorization" (str "Bearer " (:token g))
                  }
        req {:headers  headers
             :body (json/generate-string data)
             :raw-args ["--location-trusted"]
             }
        ]
    (curl/post url req)
    ))

(comment
  (write g "Hello Roam!")
  (cur-daily-page)
  )

