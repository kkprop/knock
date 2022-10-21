(ns knock.wn
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
   )
  )

(def wn
  (case (System/getProperty "os.name")
    "Mac OS X" "wn"
    "wordnet"
    )
  )


(def div
  {:new #{"\n" }
   :indent #{"=>"  " -- ("}
   :vanish #{"\t"}
   })
(def rev-div
  (mapcat #(interpose (get % 0)(get % 1)) div))

(def divs
  (apply clojure.set/union (vals div)))


(defn xs-starts-with [xs sub]
  ;; return sub
  ;; return nil
  (when (every? #(= (first %) (second %))
                (partition 2 (interleave xs sub)))
    sub
     ))

(defn xs-take-while [pred coll]
  ;;like take-while, but send the whole coll to pred
  [pred coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (when (pred s)
          (cons (first s) (xs-take-while pred (rest s)))))))

(defn- max-len-strings [xs]
  (reduce #(if (> (count %1) (count %2))
             %1
             %2)
          nil xs))

;; divs is a set of strings as dividers 
(defn div-fn [divs xs]
  (->> divs
       (map #(xs-starts-with xs %))
       (remove nil?)
       (max-len-strings)
       ))

(defn- partition-by-string [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           fv (f s)
           run (if (nil? fv)
                 (cons fst (xs-take-while #(= fv (f %)) (next s)))
                 fv)]
       (cons run (partition-by-string f (lazy-seq (drop (count run) s))))))))


(defn flatten-hashmap
  ([m]
   (if (empty? m)
     []
     (flatten-hashmap (seq m) [])))
  ([m xk]
   (if (coll? m)
     (let [[k v] (first m)
           ks (cons k xk)
           nv (if (map? v)
                (flatten-hashmap v ks)
                (if (coll? v)
                  (map #(flatten-hashmap % ks) v)
                  [ks v]))]
       (if (empty? (rest m))
         [nv]
         (cons nv (flatten-hashmap (rest m) xk))
         ))
     [xk m])))

(def rev-div
  (let [kv (flatten (flatten-hashmap div))]
    kv
    ))


(defn- search [s opt]
  (let [raw (:out (sh wn s opt))
        xs (->> (partition-by-string (partial div-fn divs) raw)
                (map #(apply str %)))] 
    xs
    ))

(defn- opts [s]
  (let [raw (:out (sh wn s))]
    (->> (partition-by #{\newline \tab } raw)
         (map #(apply str %))
         (filter #(string/starts-with? % "-"))
         (map #(string/split % #", " ))
         ;omit the duplicated options
         (map first)
         )))
(defn search-wn [s]
  (let [raw (->> (opts s)
                 (map (partial search s)))]
    raw
  ))


(comment
  (search-wn "sense")
  (parsing "a\nb\nc")
  (search "sense" "-hypen")

  (search "sense" "-over")
  ;;{{[[TODO]]}} pattern (; ; ;)
  ;;{{[[TODO]]}} 

  (div-fn divs " =>ab")
 ;; {{[[TODO]]}}铪，可以写一个，一键备份当前macros到hulu里的工具了   
  (partition-by-string (partial div-fn divs) "=> a \n b \n\n c"  )
  )
