(ns knock.server
  (:require
   [clojure.string :as str]
   [cheshire.core :as json]
   [knock.utils :as utils]
   [babashka.curl :as curl]
   [org.httpkit.server :as server])
  (:import
   [java.net URLDecoder URLEncoder]))

(defonce server (atom nil))


(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 1000ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 1000)
    (reset! server nil)))


(defn- make-body [m & {:keys [headers status] :or {headers {"Content-Type" "application/json"}} :as opts}]
  (let [orig {:headers headers
              :body (json/generate-string m)
              :status status}]
    (merge orig opts)))

(defn parse-body [body]
  (if (nil? body)
    nil
    (utils/jstr-to-edn
     (slurp (clojure.java.io/input-stream body))
      ;;body
     )))

(defn parse-uri [uri]
  (URLDecoder/decode uri))

(defn run[route-fn {:keys [port] :or {port 8000} :as opts}]
  (reset! server (server/run-server route-fn opts))
  )

;; keys:
;;
(defn hello-world [{:keys [uri body headers]}]
  (let [path (parse-uri uri)]
    (cond
      (= path "/sdk")
      (make-body {:cur-path path :body (parse-body body)
                  :headers headers
                  })
      :else

      {:status 200 :body "Hello world"})))


(defn local-call [fn-name req]
  (let [url (str "http://127.0.0.1:16916/" (utils/force-str fn-name))]
    (curl/get url {:body (json/generate-string req)})))


(defn fuzzy-search-routes [xs s]
  (->> xs (filter #(str/includes? % s))))


;;filter a namespace list which called/like keyword
;;also can remove function names in/like the neg-words 
(defn namespaces-by [keyword & neg-words]
  (->>
   (all-ns)
   (map ns-name)
   (filter #(str/includes? (str %) keyword))
   (remove #(not-every? false? (map (fn [s] (str/includes? (str %) s)) neg-words)))
   (map symbol)
   ;;
   ))

;;select functions from a namespace called/like keyword 
(defn fns-by-namespace [keyword & neg-words]
  (->> (apply namespaces-by keyword neg-words)
       (map ns-publics)
       (apply merge)
       )
  )

;;call a function return an edn with result or with notice could help 
(defn call-fn
  ([f params]
   (let [m (meta f)
         xs (first (:arglists m))
         result (if (map? params)
                  (f params)
                  (apply f params))]
     (if (sequential? result)
       (vec result)
       result)))

  ([m-fns fn-name params]
   (let [names (->> m-fns keys (map str))
         xs (fuzzy-search-routes names fn-name)
         f (get m-fns (symbol (first xs)))]
     (if (or (= 1 (count xs))
             (and (< 1 (count xs))
                  (= (first xs) fn-name)
                  )
             )
       {:result
        (call-fn f params)}
       {:error "api not found/more than one match" :matches xs}))))


;; examples
;; fuzzy-search-routes [["abc" "def"]  "bc"]
(defn handler [tokens routes {:keys [uri body headers]
                              :as request}]
  (let [path uri
        names (->> routes keys (map str))
        fname (str/replace-first uri #"/" "")
        body (parse-body body)
        xs (fuzzy-search-routes names fname)
        f (get routes (symbol (first xs)))
        req-token (get headers "authorization")]
    (if (or (nil? tokens) (utils/in? tokens req-token))
      (if (or (= 1 (count xs))
              (and (< 1 (count xs))
                   (= (first xs) fname)))
        (make-body {:result (call-fn f body)})
        (make-body {:error "api not found"
                    :matches xs}))
      (make-body {:result  "token denied"} {:status 403}))))

(defn run-ns [{:keys [ip port
                      tokens] :or {ip "127.0.0.1"} :as opts} & namespaces]
  (run
   (partial handler tokens (->> (flatten namespaces) (map ns-publics) (apply merge)))
   opts)
  ;;
  )

(comment
;;
  (do
    (stop-server)
    ;;run namespace
    (run-ns {:ip "127.0.0.1" :port 16916} 'rte.core 'lucy.server 'lucy.emoji)
    )

  (run {:ip "127.0.0.1" :port 16916})
  (stop-server)

  (run hello-world {:port 16916})
  (run handler {:port 16916})

  ;;
  )
