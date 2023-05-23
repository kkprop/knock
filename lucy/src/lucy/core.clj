(ns lucy.core
  (:require  [ruuter.core :as ruuter]
             [clojure.core.async :as async]
             [manifold.stream :as s]
             [potemkin.utils :as u]
             [knock.utils :as utils]
             [datomic.client.api :as d]
             [babashka.curl :as curl]))


(def routes [{:path "/"
              :method :get
              :response {:status 200
                         :body (str "Hi there, Lucy is here!"
                                    )}}
             {}])

;;(def cfg {:store {:backend :file :path "/tmp/example"}})

;;(def conn (d/connect cfg))




(comment

  (http/run-server #(ruuter/route routes %) {:port 8082})

  )

(def cfg {:server-type :cloud
          :region "ap-southeast-1" ;; e.g. us-east-1
          :system "lucy-1"
          :creds-profile "default"
          :endpoint (:datomic-endpoint (utils/agora-conf))})


(def client (d/client cfg))

(def conn (d/connect client {:db-name "chat"}))

(def q #(d/q % (d/db conn)))

(def transact (partial d/transact conn))

(comment
  (d/create-database client {:db-name "chat"})
  (d/transact )
  (d/db conn)

  )
