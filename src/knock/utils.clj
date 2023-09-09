(ns knock.utils
  (:require
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [clojure.data :as data]
            ;[clojure.reflect :as cr]
   [clojure.pprint :as pp]
   [clojure.string :as str]
   [clojure.java.shell :as shell :refer [sh]]
   [cheshire.core :as json :refer :all]
   [babashka.fs :as fs]
   [babashka.curl :as curl]
   [medley.core :as m]
   [clojure.zip :as z]
   [clojure.core.async :as a]
   [clojure.edn :as edn]
   [clojure.walk :as walk]))



(def os-name (System/getProperty "os.name"))

(defn uuid []
  (java.util.UUID/randomUUID)
  )

(defn join-cmd [& cmd]
  (str/join " " cmd))

(defn show-cmd [& cmd]
  (let [s (str/join " " cmd)
        _ (println s)] s))

(defn run-cmd [& cmd]
  (sh "sh" "-c" (str/join " " cmd))
  )


(comment
  (->>
   (str/split-lines
    (:out
     (run-cmd "ls")))
   (filter #(str/ends-with? % ".json") )
   )

;;
  )

(defn join-path [& cmd]
  (str/join "/" cmd))

(defn dot-split [s]
  (str/split s #"\."))

(defn ext-name [filename]
  (last (dot-split filename))
  )
(defn file-name [filename]
  (let [cols (dot-split filename)
        n (count cols)]
    (str/join "." (take (- n 1) cols))))

;;"true if coll contains elm"
(defn in? [coll elm & {:keys [cmpfn]
             :or {cmpfn =}}]
  (some #(cmpfn % elm) coll)
  )
;; true if one of things in coll are included in elm
(defn rev-in? [coll elm & {:keys [cmpfn]
                           :or {cmpfn =}}]
  (some #(cmpfn elm %) coll)
  )


;;"true if elem are contained in one of coll"
(defn fuzzy-in? [coll elm]
  (in? coll elm :cmpfn str/includes?) 
  )

(defn rev-fuzzy-in? [coll elm]
  (rev-in? coll elm :cmpfn str/includes?))



(defn pipeline-demo [n data f]
  (let [out (a/chan)
        log (a/chan 10)
        data (a/to-chan data)
        task (fn [value channel]
               (a/go
                 (a/>! log value)
                 (a/>! channel (f value))
                 (a/close! channel)))]

    (a/go-loop [] (when-some [line (a/<! log)] (println line) (recur)))
    (a/pipeline-async n out task data)
    (time (a/<!! (a/into
                  [] out)))))


(defn async-fn [fn xs]
  
  )


;; TODO meta

;; get all the command string of shell
(defmacro make-shell-fn [name]
  (let [fname (symbol name)
        args (symbol "args")]
    `(defn ~fname [& ~args]
       (str/trim-newline
        (:out
         (apply (partial run-cmd ~name) ~args))))))


(make-shell-fn "basename")
(make-shell-fn "dirname")
(make-shell-fn "openssl")
(make-shell-fn "base64")
(make-shell-fn "curl")
(make-shell-fn "grep")

(defn re-escape [s]
  (str/escape s {\. "\\."
                 \[ "\\["
                 \] "\\]"
                 \{ "\\{"
                 \} "\\}"
                 })
  )

(defn force-re [s]
  (if (string? s)
    (re-pattern s)
    s
    ))

(defn split-by [s & {:keys [re]
                     :or {re #" "}}]
  (str/split
    s
   (force-re re)))

(defn split-pairs [s re & keys]
  (apply hash-map (interleave keys (str/split s re))))

(defn crt->pem [path]
  (openssl "x509 -inform DER -in" path)
  )


(defn cert->chain [path]
  (openssl "x509 -in" path "-text | grep 'CA Issuers - URI' |awk -FURI: '{print $2}' | xargs curl -s | openssl x509 -inform DER")
  )



;;pure sequentially list a directory
(defn ls-f1 [path]
  (drop 2
        (str/split-lines
         (:out
          (run-cmd "ls" "-f1" path)))))

(defn parse-int [s]
  (if (str/blank? s) 0 (Long/parseLong (re-find #"\A-?\d+" s))))

(defn force-int [s]
  (if (string? s) (parse-int s) s))
;(defn show-members[c]
;  (let [detail (:members (cr/reflect c))]
;
;    c))

(defn parse-float [s]
  (Float/parseFloat s)
  )

(defn force-float [s]
  (if (string? s) (parse-float s) (float s)))

(defn cur-time-str
  ([]
   (cur-time-str "yyyy-MM-dd hh:mm:ss"))
  ([fmt]
     (.format (java.text.SimpleDateFormat. fmt) (java.util.Date.))))

(defn cur-year []
  (parse-int
   (cur-time-str "YYYY")))


(defn str-to-date-by-fmt [s fmt]
  (.parse (java.text.SimpleDateFormat. fmt) s))

(defn str-to-date [s]
  (str-to-date-by-fmt s "yyyy-MM-dd")
  )

(defn str-to-datetime [s]
  (str-to-date-by-fmt s "yyyy-MM-dd"))

;;to timestamp in seconds
(defn date-to-ts [date-str]
  (/ (inst-ms (str-to-date date-str)) 1000))

(defn date-to-ms [date-str]
  (inst-ms (str-to-date date-str)))

(defn date-ms-range [date-str]
  (let [start (date-to-ms date-str)]
    {:start start
     :end (+ start 86400000)})
  )

(defn date-ts-range [date-str & days]
  (let [start (date-to-ts date-str)
        seconds (if (empty? days)
                  86400
                  (* 86400 (first days)))]
    {:start start
     :end (+ start seconds)}))


(defn from-timestamp-ns [ts-ns]
  (let [ts-ns (force-int ts-ns)]
    (java.util.Date. (/ ts-ns 1000))))

(defn from-timestamp-ms [ts-ms]
  (let [ts-ms (force-int ts-ms)]
    (java.util.Date. ts-ms)))

(defn from-timestamp [ts]
  (let [ts (force-int ts)]
    (java.util.Date. (* 1000 ts))))

;;from java.util.Date to local time
(defn to-localtime [t gmt]
  (let [gmt (- 0 gmt)
        gmt (cond
              (> gmt 0)
              (str "+" gmt)
              :else
              (str "-" (abs gmt)))]

    (java.time.LocalDateTime/ofInstant
     (.toInstant t)
      ;;this is weired I know. but it works.
      ;;set reverse zone get correct value
     (. java.time.ZoneId of (str "Etc/GMT" gmt)))))

(def fmt-date-str (java.text.SimpleDateFormat. "yyyy-MM-dd"))

(def time-fmts (->> ["MMM dd HH:mm:ss yyyy" "yyyy-MM-dd HH:mm:ss"]
                    (map #(java.text.SimpleDateFormat. %))))

(defn fuzzy-parse-time [s & {:keys [to-trim]
                           :or {to-trim ".*expire date: "}}]
  (->> time-fmts
       (map #(try (.parse % (str/replace-first s (re-pattern to-trim) ""))
                  (catch Exception e nil)))
       (remove nil?)
       (first)))

(fuzzy-parse-time "2024-06-21 18:40:14 GMT")
(fuzzy-parse-time "*  expire date: Jul 24 23:30:12 2023 GMT")


(defn cur-ts-13 []
  (inst-ms (java.util.Date.)))

(defn cur-ts []
  (quot (cur-ts-13) 1000))


(def n 0)
(defn today
  ([] (today 0))
  ([n]
   (.format fmt-date-str
            (from-timestamp (+ (cur-ts) (* n 86400))))))

(defn days [from to]
  (if (nil? to)
    ;;its a question here. nil return 0
    0
    (let [from-s (quot (inst-ms to) 1000)
          to-s (quot (inst-ms from) 1000)
          duration-s (- from-s to-s)]
      (if (< 86400 duration-s)
        (quot duration-s 86400)
        (/ (float duration-s) 86400)
      ;;
        ))))

(def days-to-now (partial days (java.util.Date.)))



(defn range-date
  ([end-str])

  ([start-str end-str]
   (range-date start-str end-str 86400))

  ([start-str end-str step-in-s]
   (let [s (date-to-ts start-str)
         e (date-to-ts end-str)]
     (->>
      (range s e step-in-s)
      (map from-timestamp)
      (map #(.format fmt-date-str %))
      ))))




(def j json/generate-string)
(def e clojure.edn/read-string)

(defn jstr-to-edn [s] (json/parse-string s true))

;; Provider information loading
(defn load-json-conf [name] (parse-string (slurp (format "resources/conf/%s.json" name))))


(defn load-json-str[s]
  (let [xs (map #(parse-string % true)
                ;;force to be vector in order to parse multiple json objects
                (str/split-lines s))]
    (if (= 1 (count xs))
      (first xs)
      xs)))

(defn load-json [path]
  (load-json-str (slurp path)))


(defn curl-any [method url & {:keys [headers body]
                              :or {headers {"Content-Type" "application/json"}}
                              :as opts}]
  (let [req (if (nil? body) {:headers headers} (assoc {:headers headers} :body (j body)))
        res (try (method url req) (catch Exception e (ex-data e)))]
    (assoc res :body
           (try
             (jstr-to-edn (:body res))
             ;;can't convert to edn, just return
             (catch Exception e (:body res))))
    ;
    ))


(def curl-get (partial curl-any curl/get))
(def curl-post (partial curl-any curl/post))

(declare force-str)

(def ip-regex #"(?:[0-9]{1,3}\.){3}[0-9]{1,3}")
(def dash-ip-regex #"(?:[0-9]{1,3}-){3}[0-9]{1,3}")


(defn parse-ips [s]
  (re-seq ip-regex s)
  )

;;support 127-0-0-1 to be extraced to be 127.0.0.1
(defn extract-ip [s]
  (let [s (if (nil? s) (force-str s) s)
        xs (re-seq ip-regex s)]
    (if (nil? xs)
      (map #(str/replace % "-" ".")
           (re-seq dash-ip-regex s))
      xs)))



(defn ip? [s]
  (not (empty? (extract-ip s)))
  )

(defn dash-ip [ip]
  (str/replace ip #"\." "-"))

(extract-ip " zen-lalan-10-11-4-229 ")
(extract-ip " zen-lalan-10.11.4.229 ")


(defn dig-domain-ips [domain]
  (extract-ip
   (:out (run-cmd "curl" "-s" "-v" (str "https://" domain ":15986 2>&1 | grep Trying"))))
  ;;
  )


;; File creating. cleaning. path assembling
(defn clean-file [f-name]
  (let [mkdir (clojure.java.io/make-parents f-name)
        cleared (spit f-name "")]
    f-name))


(defn spit-line [f s]
  (let [line (if (str/ends-with? s "\n")
               s
               (str s "\n"))]
    (spit f line :append true)
    )
  )




(defn path-str-clean[s]
  (str/replace s "\n" "_"))



(defn seq-with-prev-next [coll]
  (let [p (conj (seq coll) nil)
        cur (seq coll)
        n (concat (next (seq coll)) '(nil))
        ]
    (->> (partition 3 (interleave p cur n))
      (map #(interleave [:prev :cur :next] %))
      (map #(apply hash-map %))
      )
    )
  )

(defn extract-context-line [raw kw]
  (->> (seq-with-prev-next raw)
    (filter #(str/includes? (:cur %) kw))
    ))
;;
(defn conf-path
  ([name]
   (format "resources/conf/%s.csv" (path-str-clean name)))
  ([name & args]
   (let [dir (str/join "/" args)]
     (format "resources/conf/%s/%s.csv" (path-str-clean dir) (path-str-clean name)))))

(defn conf-path-edn
  ([name]
   (format "resources/conf/%s.edn" (path-str-clean name)))
  )

(defn load-edn [path & {:keys [readers]}]
  (if (nil? readers)
    (clojure.edn/read-string (slurp path))
    (clojure.edn/read-string {:readers readers} (slurp path))))

(defn spit-edn [path x & opts]
  (let []
    (if (seq? x)
      (apply spit path (apply list x) opts)
      (apply spit path x opts))
    (spit path "\n" :append true)
    )
  )

;;load conf from resources/conf path
(defn load-conf [name]
  (load-edn (conf-path-edn name))
  )

(defn agora-conf []
  (load-conf "agora"))

(defn load-samples [name] ((keyword name) (load-conf "samples")))

(defn load-out [name]  )
;;Notice \n will be convert to _
(defn output-path
  ([name]
   (format "resources/outputs/%s" (path-str-clean name)))
  ([name & args]
   (let [dir (str/join "/" args)]
     (format "resources/outputs/%s/%s.csv" (path-str-clean dir) (path-str-clean name)))
   ))


;; (defn cur-month-date-range []
;;   (let [now-date (java-time/local-date)
;;         y (.getYear now-date)
;;         m (.getValue (.getMonth now-date))
;;         today (.getDayOfMonth (jt/local-date))
;;         ]
;;     (list (clojure.core/format "%d-%02d-01" y m)
;;       (clojure.core/format "%d-%02d-%02d" y m today))
;;     )
;;   )

;; (defn cur-month []
;;   (let [now-date (jt/local-date)
;;         y (.getYear now-date)
;;         m (.getValue (.getMonth now-date))
;;         ]
;;     (clojure.core/format "%d-%02d" y m)
;;    ))

;; (v1 k1 v2 k2 v3 k3) -> (k1 v1 k2 v2 k3 v3...)
(defn switch-position-even-odd [l]
  (let [odd (->> l (take-nth 2))
        even (->> l (rest) (take-nth 2) ) ]
    (interleave even odd)
    )
  )

(defn write-csv-line [path cols]
  (let [sep ","]
    (if (string? cols)
      (spit path (str cols "\n") :append true)
      (spit path
        (str (str/join sep
               (->> cols (map str))) "\n" )
        :append true
        )

      )
    ))

;; to resources/output/name.csv
(defn write-lines [path coll]
  (let [p path]
    (map (partial write-csv-line p) coll)
    )
  )
(defn output-lines [name coll]
  (let [path (output-path name)
        _ (clean-file path)]
    (map (partial write-csv-line path) coll)
    )
  )

(defn output-seq-map [filename smap & selected-k]
  (let [path (output-path (str filename ".csv"))
        head (flatten (->> smap (take 1) (map keys)))
        _ (clean-file path)
        selected (set (->> selected-k ))
        ]
    (if-not (nil? selected-k)
      (do
        ;; write selected header
        (write-csv-line path selected-k)
        ;; write selected values
         (->>
           smap
           (map #(select-keys % selected-k))
           (map vals)
           (map #(write-csv-line path %))
           )
        )
      (do
        ;; write header
        (write-csv-line path head)
        ;; write values
        (->> smap
              (map vals)
              (map #(write-csv-line path %))
            )
          )
      )

    ))

(defn output-seq [filename m]
  (let [path (output-path filename)
        head (->> m (take 1) (map keys) (flatten))
        _ (clean-file path)
        ]
    (write-csv-line path head)
    (->> m (map vals) (map #(write-csv-line path %)))
    )
  )

(defn ip-2-int [s]
  (->>
   (str/split s #"\.")
   (map #(Integer/parseInt %))
   (map #(format "%x" %))
   (apply str "0x")
   (read-string)))

(defn int-2-ip [i]
  (->> (Integer/toHexString i)
       (partition 2)
       (map #(apply str "0x" %))
       (map read-string)
       (str/join ".")))

(defn ip+n [n s]
  (->>
   (str/split s #"/"
                         (first))
   (ip-2-int)
   (+ n)
   (int-2-ip)))

(defn instance-to-ip [s]
  (let [ip (second (re-find #".*-(\d+-\d+-\d+-\d+)" s))]
    (if (nil? ip)
      nil
      (str/replace ip #"-" "." )
      )))

(defn ip-to-instance [ip]
  (str/replace ip #"-" "." ))

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
;;this one currently is limp. same key only return one result.
(defn cherry-pick [m & ks]
  (apply hash-map
         (flatten
          (apply cherry-pick-keys m ks))))

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

(defn fuzzy-instant [n]
  (if (< n 0xFFFFFFFF)
    (from-timestamp n)
    (if (< n 4294967295000)
      (from-timestamp-ms n)
      (from-timestamp-ns n)
      )))

(defn fuzzy-pick [m k-fn]
  (->> m
       (filter #(get k-fn (first %)))
       (map (fn [[k v]] {k ((get k-fn k) v)}))
       (apply merge)))


(defn val-diff [a b vcf k]
  (vcf (get a k) (get b k)))

;;differences of tree hierarchy
;;a b two hash-map
;;vcf value comparing function 
(defn tree-diff [a b vcf]
  (let [;a (when-not (seq? a) {:no-key a})
        ;b (when-not (seq? b) {:no-key b})
        flat-a  (apply merge (flatten-hashmap a))
        flat-b (apply merge (flatten-hashmap b))
        ak (set (keys flat-a))
        bk (set (keys flat-b))
        [a-only b-only comm] (clojure.data/diff ak bk)
        diff-fn (partial val-diff flat-a flat-b vcf)
        detail (->> comm
                    (map #(list % (diff-fn %)))
                    (filter #(false? (last %)))
                    (map first)
                    (reduce (fn [a c]
                              (assoc a c {:a (get flat-a c)
                                          :b (get flat-b c)})) {}))
        ]
    (if (and (nil? a-only) (nil? b-only))
      {:common-key-diff detail}
      {:a-only-key a-only
       :b-only-key b-only
       :common-key-diff detail})
    ;
    ))

(defn vcf-count[a b] (= (count a) (count b)))



(defn csv-to-hashmap [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map #(if-not (keyword? %)
                    (if (empty? %)
                      (keyword (str "nil-" (cur-ts-13)))
                      (keyword %))
                          %)) repeat)
       (rest csv-data)))

(defn load-csv [fname]
  (with-open [reader
              (io/reader fname)]
    (doall
     (csv/read-csv reader))))

(defn load-csv-to-hashmap [fname]
  (csv-to-hashmap (load-csv fname))
  )

(defn all-files [dir & [ext]]
  (->> (fs/list-dir dir)
       (map str)
       (filter #(if (nil? ext)
                  true
                  (str/ends-with? % ext)))))

(defn load-all-csv [dir]
  (->>
   (all-files dir ".csv")
   (reduce (fn [a c]
             (assoc a c (load-csv-to-hashmap c))) {})))

(defn fuzzy-matched? [s kw]
  (let [p (re-pattern (format "(?i).*%s.*" kw))]
    (re-matches p s)))

(defn make-url [prefix path & kv]
  (let [kv-pairs (flatten
                  (map #(if (map? %) (flatten (seq %)) %) kv))
        params (->> kv-pairs
                    (partition 2)
                    (map #(str (name (first %))
                               "="
                               (if (keyword? (second %))
                                 (str (name (second %)))
                                 (str (second %)))))

                    (str/join "&&")
                    (apply str))]

    (if (empty? params)
      (str prefix path)
      (str prefix path "?" params)
      )
    ))

;;Mix :k "v" {:mk "mv" :mmk "mmv"}
(defn kv-pair [& xs]
  (->>
   (flatten (map #(if (map? %)
                    (flatten (seq %))
                    %)
                 xs))
   (apply hash-map)
   ;
   ))

(defn force-keyword [x]
  (if (keyword? x)
    x
    (keyword x)
    )
  )

(defn force-str [x]
  (if (string? x)
    x
    (if (keyword? x)
      (name x)
      (str x))))

(defn word-capitial->dash [x]
  (let [s (force-str x)
        words (str/split (str/replace s #"\p{Lu}" " "))]
    (str/join "-" words)
    )
  )

(defn dash->word-capital [x]
  (let [s (force-str x)
        words (str/split s #"-")
        ]
    (apply str (map str/capitalize words))
    )
  )

(defn dash->camel-case [x]
  (let [s (force-str x)
        words (str/split s #"-")
        [first & left] words]
    (apply str first (map str/capitalize left))))

(comment
  (dash->camel-case "secret-access-key")
  )


(defn slash-keys [& xs]
  (if (nil? xs)
    nil
    (keyword (str/join "/" (map #(force-str %) xs)))
    )
  )

(defn slash-list [xs]
  (apply slash-keys (reverse (flatten xs))))

;;rude way of flatten members with list will only have one. lost others
(defn slash-flatten-map [m]
  (->> (flatten-hashmap m)
       (map #(map-on-key (fn [x] (apply slash-keys x) ) %))
       (apply merge)
       )
  )
(defn slash-keep-last [m]
  (map-on-key #(keyword (last (str/split (force-str %) #"/"))) m))

(defn all-the-same?
  ([xs]
   (all-the-same? xs =))
  ([xs f]
   (let [x (first xs)]
     (every? #(f x %) xs))))

(defn longest-match [& xss]
  (let [n (apply max (map count xss))]
    (loop [i 0]
      (if (= i n)
        ;;(map #(partition-all n %) xss)
        i
        (if-not (all-the-same?
                 (flatten
                  (map #(nth % i nil) xss)))
          i
          (recur (+ i 1))))
          ;;
      )))



(def db-id (atom -1))
(defn reset-id []
  (reset! db-id 0))
(defn seq-id []
  (swap! db-id dec )
  )

(defn e-kv [[k v]]
  (println k v)
  )

(defn entities
  ([m]
   (let [_ (reset-id)]
     (map second (entities m {:db/id (seq-id)
                              :pid -1
                              :key []}))))
  ([m context]
   (let [xs (tree-seq map? identity m)
         kvs (drop 1 xs)
         id (:db/id context)]
     (if (= 1 (count xs))
       {:context context}
       (->> kvs
            (mapcat (fn [[k v]]
                      (if (sequential? v)
                        ;;mapcat a list v
                        (mapcat #(entities % (assoc context
                                                    :db/id (seq-id)
                                                    :p2c-key k
                                                    :key (conj (:key context) k)
                                                    :is-list true
                                                    :pid id
                                                    :v %)) v)
                        (if (map? v)
                          (if (< 1 (count v))
                          ;;multiple key hash map
                            (let [cur-id (seq-id)]
                              (entities v (assoc context
                                                 :db/id cur-id
                                                 :key (conj (:key context) k)
                                                 :pid id
                                                 :p2c-key k
                                                 :is-list false
                                                 :v v)))
                          ;;single key hash map
                            (entities v (assoc context
                                               :db/id id
                                               :key (conj (:key context) k)
                                               :v v)))
                          (entities v
                                    (assoc context
                                           :db/id id
                                           :key (conj (:key context) k)
                                           :v v)))))))))))

(defn suffix-list [xs x]
  (loop [xs xs]
    (if (empty? xs)
      nil
      (if (= x (first xs))
        (rest xs)
        (recur (rest xs))))))

(defn prefix-list [xs x]
  (loop [xs xs c []]
    (if (= x (first xs))
      (conj c x)
      (recur (rest xs) (conj c (first xs)))
    )
  ))

(defn slash-suffix-list [xs x]
    (apply slash-keys (suffix-list xs x)))

(defn slash-prefix-list [xs x]
    (apply slash-keys (prefix-list xs x)))


(defn cat-kvs [{:keys [key v pid
                       p2c-key
                       layer-key]
                :as m}]
  (let [id (:db/id m)]
    (if (nil? p2c-key)
      [{:db/id id
        :key (apply slash-keys key)
        :v v}]
      [{:db/id id
        :key (slash-suffix-list key p2c-key)
        :v v}
       {:db/id pid
        :key (slash-prefix-list key p2c-key)
        :is-list (:is-list m)
        ;;put self into pid's list
        :v [{:db/id id :key (slash-suffix-list key p2c-key) :v v}]}])))

(defn merge-kvs [kvs]
  (let []
    (->> kvs
         (mapcat cat-kvs)
         )
    )
  )

(defn merge-vals [xs]
  (let [is-list (some true? (map :is-list xs))
        new-v (->> (map :v xs)
                   (flatten)
                   (map #(if (nil? (:key %))
                           ;;don't have to make another layer 
                           (:v %)
                           ;;is an object have to make another layer
                           {:db/id (:db/id %)}))
                   distinct
                   vec)
        m (first xs)]

    (if is-list
      ;;is list merge objects
      (assoc (first xs) :v new-v :new-v true)
      ;;is map single val
      (if (= 1 (count xs))
        (if (nil? (:is-list m))
          m
          ;;a map nest another map
          (map-on-val #(if (sequential? %)
                         (first %)
                         %
                         ) m)
          )
        (assoc (first xs) :v new-v :new-v true)))))

(defn merge-entity-key [[id xs]]
  (let []
    (flatten
     (vals
      (map-on-val merge-vals (group-by :key xs))))
    ;;(group-by :key xs)
    ))

(defn batch-uuid [ids]
  (->> ids
       (map (fn [id] {:id (uuid)}))
       (apply
         merge)))

;;datascript way of flatten a hashmap to be entities
;; each layer is a new entity unless all layers is single key value
(defn flatten-entities [m]
  (let [ids (group-by :db/id (entities m))
        ;;entities (reverse (sort-by first (group-by :db/id (mapcat merge-kvs (vals ids)))))
        entities (mapcat merge-kvs (vals ids))
        id-e (group-by :db/id entities)
        ]
    (->>
     (mapcat merge-entity-key id-e)
    ;;nil key aready have it self int it's pid's field
     (remove #(nil? (:key %)))
     ;;(map (fn [x]{:db/id (:db/id x) (:key x) (:v x)}))
     )))

(defn full-link-flatten [m]
  (->> m
       )
  )


(comment

  (def i 4)
  (def xs '({[:publicIpv4 :subnet :isp :name] cucc}))

  (def xs '({[:meta :bin] "Yin"} {[:meta :bin] "Yang"}))

  (def i 2)
  (def xss
    [[1 2 3] [1 2]])
  (all-the-same?
   (map #(nth % 2 nil) xss))

  (map #(partition-all  i %) xss)
  (partition-all 2 xss)
;;
  )


;;should select pickup 
(defn select [m & keys]
  (select-keys m keys)
  )

(defn map-zip [mapping]
  (z/zipper
   (some-fn map-entry? map?)
   (fn [x]
     (cond
       (map? x) (seq x)

       (and (map-entry? x)
            (-> x val map?))
       (-> x val seq)))
   nil
   mapping))

;;TODO
(defn sub-tree-selection [ m & kvs]
  )


(defn select-or-get [m k-or-keys]
  (if (sequential? k-or-keys)
    (select-keys m k-or-keys)
    (get m k-or-keys)))

(defn exfoliation [m & ks]
  (let [k (first ks)]
    (if (empty? ks)
      m
      (if (map? m)
        (apply exfoliation
               (select-or-get m k)
               (rest ks))
        (map #(apply exfoliation
                     (select-or-get % k)
                     (rest ks)) m)))))

;;break a large set into pages
(defn page-select [xs current page-size]
  (let [cols (partition-all page-size xs)
        ;;for frontend players they seem to like use 1 as start of index
        res (nth cols (- current 1) '())]
    {:list res
     :total (count cols)
     :current current
     :pageSize page-size}))

;; A Paginator :: could apply to AWS API
;; use last returned elements for next page indexing
(defn page-query
  ([f]
   (page-query f {:p {} :cache []}))
  ([f context]
   (let [curRes (f (:p context))]
     (if (empty? curRes) 
       (:cache context)
       (recur f {:p curRes
                 :cache (conj (:cache context) curRes)
               })))))

;; load from default config file
;;  i.e. (config :chrome-profile)
;;       a varible chrome-profile is defined
(defmacro config [name & {:keys [config-path]
                          :or {config-path "resources/config.edn"}}]
  (let [var-name (symbol (force-str name))
        key-word (keyword (force-str name))]
    `(def ~var-name (~key-word (load-edn "resources/config.edn")))))


(defn dash-dash-kv [m & selected-keys]
  (let [m (if (nil? selected-keys)
            m
            (select-keys m selected-keys))]
    (->> m
         (map (fn [[k v]]
                (str "--" (force-str k) " " (force-str v)))
              )
         (str/join " ")
         )
    ))


(comment
  (dash-dash-kv {:x 'x1 :y 'y1} )
  (tree-diff a b vcf-count)
  (flatten-hashmap  a)
  (flatten-hashmap  nest-a)

  (map keys
       (flatten-hashmap nest-a))
  )



;;surely have issue of performance
;;from 
(defn var-meta [f]
  (meta (second (first (filter #(and (var? (second %)) (= f (var-get (second %)))) (ns-map *ns*))))))


(defn- mock-context [f & args]
  (let [fm (var-meta f)
        ns-path (str/replace (ns-name (:ns fm)) #"\." "/")
        dir (str "resources/mock/" ns-path "/")
        path (if (empty? args)
               (str dir (:name fm) ".edn")
               (str dir (:name fm) "/" (str/join "-" (map force-str args)) ".edn"))
        tmp-path (str path "." (cur-time-str "yyyy-MM-dd-hh:mm:ss.SSS"))]
    {:fm fm
     :ns-path ns-path
     :dir dir
     :path path
     :tmp-path tmp-path}
    ))
;;for a slow return function.
;;  mock will run once save the result to local edn file
;;  the next call of mock will read local file
;;call mock-clean to clean local file
(defn mock [f & args]
  (let [{:keys [fm ns-path dir path tmp-path]} (apply mock-context f args)
        _ (clojure.java.io/make-parents path)]
    (if (fs/exists? path)
      (try
        (load-edn path)
        ;; when failed try call function again and cache
        (catch Exception e
          (let [res (apply f args)
                _ (fs/delete-if-exists path)] (clojure.pprint/pprint res (clojure.java.io/writer path)) res)))
      (let [res (apply f args)
            _ (clean-file tmp-path)]
        (clojure.pprint/pprint res (clojure.java.io/writer tmp-path))
        (fs/delete-if-exists path)
        (fs/create-sym-link (fs/absolutize path) (fs/absolutize tmp-path))
        res)
      ;;
      )))

(defn mock-clean [f & args]
  (let [{:keys [fm ns-path dir path]} (apply mock-context f args)]
    (when (fs/exists? path)
      (fs/delete path))))


(defn choose [xs]
  (let [_ (apply list (map-indexed (fn [i x] (println i x)) xs))
        _ (println "choose the index want to use")
        i (force-int (read-line))]
    (nth xs i nil)))

(defn mock-choose [f & args]
  (let [{:keys [fm ns-path dir path]} (apply mock-context f args)
        xs (map str (fs/glob (dirname path) (str (basename path) ".*")))
        tmp-path (choose xs)]
    (when-not (nil? tmp-path)
      (fs/delete-if-exists path)
      (fs/create-sym-link path (basename tmp-path))
      )))

(defn slow-repeater [x n]
  (let []
    (Thread/sleep (* 1000 n))
    (take n (repeat x))
    ))

(defn curl-cert-expire-date [domain ip port]
  (fuzzy-parse-time
   (curl "-vI" "--resolve" (str domain ":" port ":" ip)
         (str "https://" domain ":" port) "2>&1" "| grep 'expire date' " "| cut -d: -f2- | xargs")))

(defn percentage [total numerator & {:keys [percision]
                                     :or {percision 4}}]
  (format (str "%." percision "f") (/  numerator (float total))))


(defn while-change [cb f & args]
  (let []
    (loop [prev nil cur (apply f args)]
      (when-not (= nil cur) (cb cur))
      (recur cur (apply f args))
      )))

;;initial with time println the change
(def while-print-change (partial while-change #(println (cur-time-str) " ")))
;;initial with time println the change
(def while-print-change (partial while-change #(println (cur-time-str) " " %)))


;; start end pairs
(defn char-in-range? [start end x]
  (let [n (int x)]
    (and (<= (int start) n)
         (<= n (int end)))))

(defn range-replace-string [ s to-char start end ]
  (->> s
       (map #(if (char-in-range? start end %)
               to-char
               %))
       (apply str)
       ))


(defn i-pprint [x & {:keys [focus]}]
  (let [_ (clojure.pprint/pprint x)
        _ (when-not (nil? focus)
            (println "")
            (println (focus x))
            (println ""))
        input (read-line)]
    (if (map? x)
      (assoc x :input input)
      input)))


(defn comm [a b]
  (let [sa (set a)
        sb (set b)]
    {:a-only (clojure.set/difference sa sb)
     :b-only (clojure.set/difference sb sa)
     :both (clojure.set/intersection sa sb)}))

(declare fib)

(defn fib-seq
  "Returns a lazy sequence of Fibonacci numbers"
  [a b]
  (lazy-seq (cons a (fib-seq b (+ (bigint a) b)))))

(def fib (fib-seq 0 1))


;;filter a namespace list which called/like keyword
;;also can remove function names in/like the neg-words 
(defn namespaces-by [keyword & neg-words]
  (->>
   (all-ns)
   (map ns-name)
   (filter #(str/includes? (str %) (force-str keyword)))
   (remove #(not-every? false? (map (fn [s] (str/includes? (str %) s)) neg-words)))
   (map symbol)
   ;;
   ))

;;select functions from a namespace called/like keyword 
(defn fns-by-namespace [keyword & neg-words]
  (->> (apply namespaces-by keyword neg-words)
       (map ns-publics)
       (apply merge)))


(defn gen-bb-tasks [ns]
  (fns-by-namespace ns)
  )


;; 
(defn mapcat-key [m k]
  (if (sequential? (k m))
    (->> (k m)
         (map #(assoc m k %)))
    [m]))


(comment

  (-> "abcAB.*?!"
      (range-replace-string "_" \A \Z)
      (range-replace-string "_" \a \z)
      (range-replace-string "_" \0 \9)
      )

  (curl-cert-expire-date "www.163.com" "111.177.39.150" 443)

  (mock slow-repeater "abc" 3)

  (mock slow-repeater "abc" 6)

  (mock-clean slow-repeater "abc" 3)
  ;;
  )
