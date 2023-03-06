(ns knock.server
  (:require
   [clojure.string :as str]
   [cheshire.core :as json]
   [knock.utils :as utils]
   [org.httpkit.server :as server])
  )

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 1000ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 1000)
    (reset! server nil)))

(defn make-body [m & {:keys [headers]
                       :or {headers {"Content-Type" "application/json"}}
                       :as opts}]
  (let [orig {:headers headers
              :body (json/generate-string m)
              }]
    (merge orig opts)))

(defn parse-body [body]
  (if (nil? body)
    nil
    (utils/jstr-to-edn
     (slurp (clojure.java.io/input-stream body)))))


(defn run[route-fn {:keys [port] :or {port 8000} :as opts}]
  (reset! server (server/run-server route-fn opts))
  )
;; keys:
;;
(defn hello-world [{:keys [uri body headers]
                    :as request}]
  (let [path uri]
    (cond
      (= path "/sdk")
      (make-body {:cur-path path
                  :body (parse-body body)
                  :headers headers
                  :body-raw body
                  })
      :else
      {:status 200 :body "Hello world"})))

(defn handler [request]
  (spit "log.txt" request )
  {:status 200 :body "ok"}
  )


(comment
  (run hello-world {:port 16916})
  (run handler {:port 16916})
  )
