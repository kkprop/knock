(ns search.wn
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [clojure.zip :as z]
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
   :vanish #{"\t" "--------------" "       "}
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
  (let [kv (flatten (flatten-hashmap div))
        vk (interleave (take-nth 2 (rest kv)) (take-nth 2 kv))
        ]
    (apply hash-map vk)
        ))


(defn- zp-append-right [zp x]
  (assoc zp :cur (-> (:cur zp) z/rightmost (z/insert-right []) z/rightmost (z/insert-child x)))
  )

(defn- build-layers
  ([xs]
   (let [zp (z/vector-zip [[]])
         ]
     (if (empty? xs)
       zp
       (z/root
        (:cur (build-layers xs {:backtrace [] :cur (z/down zp)})))
       )
     )
   )
  ([xs zp]
   (let [x (first xs)
        props (get rev-div x)
        ]
    (if (empty? (rest xs))
      (case props
        :new (assoc zp
                    :cur (z/rightmost ((apply comp (:backtrace zp)) (:cur zp)))
                    :backtrace '()
                    )
        :indent (assoc zp
                       :cur (-> (:cur zp) z/rightmost z/down)
                       :backtrace (conj (:backtrace zp) z/up ) 
                       )
        :vanish  (build-layers (rest xs) zp)
        nil  (assoc zp :cur (-> (:cur zp) z/rightmost (z/insert-right []) z/rightmost (z/insert-child x)))
        (build-layers (rest xs)
                      (assoc zp :cur (-> (:cur zp) z/rightmost (z/insert-right []) z/rightmost (z/insert-child x)))
                      )
        )
      (case props
        :new (if (string/starts-with? (first (rest xs)) " " )
               (build-layers (rest xs) zp)
               (build-layers (rest xs) (assoc zp
                                              :cur (-> ((apply comp (:backtrace zp)) (:cur zp))
                                                       z/rightmost 
                                                       )
                                              :backtrace '()
                                              ))
               )
        :indent (build-layers (rest xs)
                              (assoc zp
                                     :cur (-> (:cur zp) z/rightmost z/down )
                                     :backtrace (conj (:backtrace zp) z/up ) 
                                     )
                              )
        :vanish (build-layers (rest xs) zp)
        nil (build-layers (rest xs)
                          (assoc zp :cur (-> (:cur zp) z/rightmost (z/insert-right []) z/rightmost (z/insert-child x)))
                          )
        (build-layers (rest xs)  
                      (assoc zp :cur (-> (:cur zp) z/rightmost (z/insert-right []) z/rightmost (z/insert-child x))) 
                      ))
    ))))


(defn- search [s opt]
  (let [raw (:out (sh wn s opt))
        xs (->> (partition-by-string (partial div-fn divs) raw)
                (map #(apply str %)))] 
    (->>
     (build-layers xs)
     ;too rude. need to fix
     (remove #(or (< (count %) 1) ) )
     )
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
  (let [raw (->> (opts s) (map (partial search s)))]
    raw
  ))


(comment
  (build-layers
   (search "feeling" "-hypen"))
  (z/vector-zip (z/root (build-layers ["\n" "abc" "\n"] )))
  (defn-test t-build-layers []
    (build-layers [])
    (build-layers ["abc"])
    (build-layers ["abc" "\n"])
    (build-layers ["abc" "def" "\n"])
    (build-layers ["abc" "def" "=>" "ghi" "=>""jkl"] )
    (build-layers ["abc" "def" "=>" "ghi" "  " "=>""jkl"  "  "] )
    (build-layers ["abc" "def" "=>" "ghi" "=>" "jkl"
                            "\n" "mno"])
    (build-layers ["abc" "def" "=>" "\n" "ghi" "=>" "\n" "jkl"])
    (build-layers [])
    )
  (search-wn "cultivate")
  (parsing "a\nb\nc")
  (search "feeling" "-hypen")
  (search "sense" "-over")

  ;;{{[[TODO]]}} 
  (div-fn divs " =>ab")
  (partition-by-string (partial div-fn divs) "=> a \n b \n\n c"  )
  )

