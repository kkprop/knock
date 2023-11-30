(ns lucy.core
  (:require  [ruuter.core :as ruuter]
             [clojure.core.async :as async]
             [potemkin.utils :as u]
             [knock.utils :as utils]
             [datomic.client.api :as d]
             [lanterna.screen :as s]
             [datomic-schema.schema :as s :refer [defdbfn]]
             [babashka.curl :as curl]))


(def routes [{:path "/"
              :method :get
              :response {:status 200
                         :body (str "Hi there, Lucy is here!"
                                    )}}
             {}])

(comment

  (http/run-server #(ruuter/route routes %) {:port 8082})

  )

(def cfg {:server-type :cloud
          :region "ap-southeast-1" ;; e.g. us-east-1
          :system "lucy-1"
          :creds-profile "default"
          :endpoint (:datomic-endpoint (utils/agora-conf))})

(def client (d/client cfg))

(def dbs (atom {}))

(defn load-db [db-name]
  (d/create-database client {:db-name db-name})
  (d/connect client {:db-name db-name})
  )
(defn db [path]
  (get @dbs path
       (let [cur (load-db path)]
         (swap! dbs merge {path cur})
         cur)))

(defn to-add-expr [xs]
  (->> xs
       (map (fn [m]
              [:db/add (:db/id m)
               (:key m) (:v m)
               ])
            )
       ))

(comment
  (d/q '[:find ?c ?i
         :where [?e :name ?c]
         [?e :chat_id ?i]]
       (d/db (db "chat")))

  (d/pull
    (db "chat")
    '[*])
;;
  )

(def parts
  (s/generate-parts   [(s/part "app")])
  )

(def group (s/schema group
                     (s/fields
                      [name :string :indexed]
                      [chat_id :string :indexed]
                      [company_name :string]
                      [owner :string]
                      [create_time :inst]
                      [update_time :inst])))

(def message (s/schema message
                       (s/fields
                         [roomid :string :indexed]
                         [msgtime :inst]
                         [msgtype :string]
                         [content :string]
                         [company_name :string]
                         [tolist :string :many]
                         [from :string :indexed]))
  )
(def schema
  [group
   message]
  )

(defn entity [db-name e]
  )

(defdbfn dbinc [db e a qty] :db.part/user
  [[:db/add e a (+ qty (or (get (entity db e) a) 0)) ]])


(defn init [db-name schema]
  (d/transact (db db-name)
              {:tx-data
               (s/generate-schema schema)
               }
              ;;
              ))

(comment
  (init "chat" group)
  (init "chat" message)
  (d/pull
   (d/db (db "chat"))
   '[:db/id] 3)

  (d/delete-database client
                     (d/db (db "chat")))
  ;;
  )

(defn kfn-schema [schema]
  (->> (:fields schema)
      (utils/map-on-val (fn [v] clojure.core/identity))
      (utils/map-on-key utils/force-keyword)))

(defn fuzzy-pick-by-schema [schema m & k-fn]
  (let [default-k-fn (kfn-schema schema)
        k-fn (merge default-k-fn k-fn)
        ]
    (utils/fuzzy-pick m k-fn)
    ))




(comment
  ()
  )


;;to early to expose
;;(def q #(d/q % (d/db conn)))
;;(def transact (partial d/transact conn))
;;(def pull #(d/pull (d/db conn) %))
;;(def as-of #(d/as-of (d/db conn) %))
;;(def since #(d/since (d/db conn) %))
