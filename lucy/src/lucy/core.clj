(ns lucy.core
  (:require  [ruuter.core :as ruuter]
             [clojure.core.async :as async]
             [potemkin.utils :as u]
             [knock.utils :as utils]
             [datomic.client.api :as d]
             [datomic-schema.schema :as s]
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

  (def chat-schema
    [{:db/ident :name
      :db/valueType :db.type/string
      :db/cardinality :db.cardinality/one}
     {:db/ident :chat_id
      :db/valueType :db.type/string
      :db/cardinality :db.cardinality/one}])

  (d/transact (db "demo") {:tx-data chat-schema})

  (d/transact (db "demo") {:tx-data [{:name "聊天1"
                                      :chat_id "111"}
                                     {:name "聊天2"
                                      :chat_id "222"}
                                     {:name "聊天3"
                                      :chat_id "333"
                                      :doc "doc3"}]})

  (d/q '[:find ?c ?i
         :where [?e :name ?c]
         [?e :chat_id ?i]]
       (d/db (db "demo")))

;;
  )

(def parts
  (schema/generate-parts   [(s/part "app")])
  )

(def schema
  [(s/schema group
             (s/fields
              [name :string :indexed]
              [chat_id :string :indexed]
              [company_name :string]
              [owner :string]
              [create_time :inst]
              [update_time :inst]))

   (s/schema message
             (s/fields
              [roomid :string :indexed]
              [msgtime :inst]
              [msgtype :string]
              [content :string]
              [tolist :string :many]
              [from :string :indexed]))]
  )


(defn chat-message []
  (->> (utils/load-json "2023-05-18.json")
       (take 2)
       (map (comp :origindata :_source))
       (map utils/jstr-to-edn)
       ;;(map utils/flatten-entities)
       )
  ;;
  )


(defn chat-group []
  (->> (utils/load-json "chat_group.json")
       (take 2)
       (map (comp :_source))
       ;;(map :doc)
       ;;(map utils/flatten-entities)
       ;;(map to-add-expr)
       ;;(map #(d/transact (db "chat") {:tx-data %}))
       ;;
       )
  ;;
  )


;;to early to expose
;;(def q #(d/q % (d/db conn)))
;;(def transact (partial d/transact conn))
;;(def pull #(d/pull (d/db conn) %))
;;(def as-of #(d/as-of (d/db conn) %))
;;(def since #(d/since (d/db conn) %))





(comment
  (d/transact)
  (d/db conn)

  (d/q {:query '[:find :block/uid]
        :args [(d/db conn)]})

  (transact {:tx-data [{:db/ident :block/uid
                        :db/valueType :db.type/uuid
                        :db/cardinality :db.cardinality/one}]})

  (def name-schema
    [{:db/ident :block/uid
      :db/valueType :db.type/string
      :db/cardinality :db.cardinality/one
      :db/doc "A person's name"}])

  (d/create-database client {:db-name "demo"})
  (def conn (d/connect client {:db-name "demo"}))
;; using a connection named conn:
  (d/transact conn {:tx-data name-schema})
  )
