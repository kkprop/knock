(ns knock.utils
  (:require [clojure.java.io :as io]
            [cheshire.core :as json]
            )
  )


(defn load-edn [path & {:keys [readers]}]
  (if (nil? readers)
    (clojure.edn/read-string (slurp path))
    (clojure.edn/read-string {:readers readers} (slurp path))))

(defn load-json [path] (json/parse-string (slurp path)))


(defn cur-time-str
  ([]
   (cur-time-str "yyyy-MM-dd hh:mm:ss")
   )
  ([fmt]
   (.format (java.text.SimpleDateFormat. fmt) (java.util.Date.))
   )
  )


(defn map-on-key [f m]
  (zipmap
   (map f (keys m))
   (vals m)))

(defn map-on-val [f m]
  (zipmap
   (keys m)
   (map f
   (vals m))))

(defn cherry-pick-keys [m & ks]
  (let [hs (apply hash-set (map keyword ks))]
    (->> (tree-seq coll? identity m)
         (filter #(and (map-entry? %) (contains? hs (keyword (first %)))))
    )))

(defn flatten-hashmap
  ([m]
   (if (empty? m)
     []
     ;;here flatten then [{ks v}]
     (->> (flatten (flatten-hashmap (seq m) []))
          (map #(map-on-key reverse %))
          )))

  ([m xk]
   (if (coll? m)
     (let [[k v] (first m)
           ks (cons k xk)
           nv (if (map? v)
                (flatten-hashmap v ks)
                (if (coll? v)
                  (if (or (map? v) (map? (first v)))
                    (map #(flatten-hashmap % ks) v)
                    [{ks v}])
                  [{ks v}]))]

       (if (empty? (rest m))
         nv
         (cons nv (flatten-hashmap (rest m) xk))))
     {xk m})))

(defn fuzzy-pick-map [m & ks]
  (let [hs (apply hash-set ks)]
    (->> (flatten-hashmap m)
         ;; for 
         (remove #(empty? (clojure.set/intersection hs (apply hash-set (first %))))))))


