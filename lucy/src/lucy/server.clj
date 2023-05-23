(ns lucy.server
  (:require [knock.server :as server]
            [clojure.string :as str]
            [com.wsscode.edn-json :refer [edn->json-like json-like->edn] ]
            [cheshire.core :as json]
            [knock.utils :as utils]))


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

;;call a function return an edn with result  or with notice could help
(defn call-fn
  ([f params]
   (let [m (meta f)
         xs (first (:arglists m))
         result (if (map? params)
                  (f params)
                  (apply f params))]
     (if (seq? result)
       (vec result)
       result
       )))

  ([m-fns fn-name params]
   (let [names (->> m-fns keys (map str))
         xs (fuzzy-search-routes names fn-name)
         f (get m-fns (symbol (first xs)))]
     (if (= 1 (count xs))
       {:result
        (call-fn f params)}
       {:error "api not found/more than one match" :matches xs}))))


;; examples
;; fuzzy-search-routes [["abc" "def"]  "bc"]
(defn handler [routes {:keys [uri body headers]
                         :as request}]
  (let [path uri
        names (->> routes keys (map str))
        fname (str/replace-first uri #"/" "")
        body (server/parse-body body)
        xs (fuzzy-search-routes names fname)
        f (get routes (symbol (first xs)))]
    (if (= 1 (count xs))
      (server/make-body {:result (call-fn f body)})
      (server/make-body {:error "api not found"
                         :matches xs}))))

(defn run [{:keys [ip port] :or {ip "127.0.0.1"} :as opts} & namespaces]
  (server/run
   (partial handler (->> (flatten namespaces) (map ns-publics) (apply merge)))
   opts)
  ;;
  )


(defn parse-edn-body [body]
  (if (nil? body)
    nil
    (json-like->edn
      (json/parse-string
        (slurp (clojure.java.io/input-stream body))
        false
        )))
  )

;; examples
;; fuzzy-search-routes [["abc" "def"]  "bc"]
(defn edn-handler [routes {:keys [uri body headers]
                       :as request}]
  (let [path uri
        names (->> routes keys (map str))
        fname (str/replace-first uri #"/" "")
        body (parse-edn-body body)
        xs (fuzzy-search-routes names fname)
        f (get routes (symbol (first xs)))
        _ (println body)
        ]
    (if (= 1 (count xs))
      (server/make-body {:result (edn->json-like (call-fn f body))}
                        :notice "unmarshall with json-like->edn using com.wsscode/edn-json"
                        )
      (server/make-body {:error "api not found"
                         :matches xs}))))

(defn edn-run [{:keys [ip port] :or {ip "127.0.0.1"} :as opts} & namespaces]
  (server/run
   (partial edn-handler (->> (flatten namespaces) (map ns-publics) (apply merge)))
   opts)
  ;;
  )

(def stop-server server/stop-server)

(comment
;;
  (do
    (server/stop-server)
    (run {:ip "127.0.0.1" :port 16916} 'rte.core 'lucy.server 'lucy.emoji))

  (run {:ip "127.0.0.1" :port 16916} )
  (server/stop-server)

  (handler
   (ns-publics 'lucy.server)
   {:uri "/fuzzy-search-"
    :body [["abc" "def"]
           "bc"]
    :headers nil})

  (call-fn
   (second (first (ns-publics 'lucy.server)))
   [["abc" "def"] "ab"])

  (call-fn
    (fns-by-namespace "lucy")
    "rand-n-emoji"
    [3]
    )

  (call-fn
   #'lucy.emoji/rand-nth
   [3])

  (call-fn #'lucy.server/fuzzy-search-routes [["abc" "def"]  "bc"])

  (fns-by-namespace "lucy")

  (json-like->edn
      (json/parse-string 
        (utils/j
          (edn->json-like {:a/b/c {:e/f :abc}}))
        ) false)
    )
;;
