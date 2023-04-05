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
   [clojure.zip :as z]
   [clojure.core.async :as a]
   [clojure.edn :as edn]))

(def os-name (System/getProperty "os.name"))

(defn join-cmd [& cmd]
  (str/join " " cmd))

(defn show-cmd [& cmd]
  (str/join " " cmd))

(defn run-cmd [& cmd]
  (sh "sh" "-c" (str/join " " cmd))
  )

(defn join-path [& cmd]
  (str/join "/" cmd))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))


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
  (let [fname (symbol name)]
    `(defn ~fname [& args]
       (apply (partial run-cmd ~name) args))))

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
    (java.util.Date. (/ ts-ns 1000000))))

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


(defn cur-ts-13 []
  (inst-ms (java.util.Date.)))

(defn cur-ts []
  (quot (cur-ts-13) 1000))


(def j json/generate-string)
(def e clojure.edn/read-string)

(defn jstr-to-edn [s] (json/parse-string s true))

;; Provider information loading
(defn load-json [name] (parse-string (slurp (format "resources/conf/%s.json" name))))

(def ip-regex #"(?:[0-9]{1,3}\.){3}[0-9]{1,3}")
(def dash-ip-regex #"(?:[0-9]{1,3}-){3}[0-9]{1,3}")

(defn extract-ip [s]
  (let [xs (re-seq ip-regex s)]
    (if (nil? xs)
      (map #(str/replace % "-" ".")
           (re-seq dash-ip-regex s))
      xs)))

(extract-ip " zen-lalan-10-11-4-229 ")
(extract-ip " zen-lalan-10.11.4.229 ")

;; File creating. cleaning. path assembling
(defn clean-file [f-name]
  (let [mkdir (clojure.java.io/make-parents f-name)
        cleared (spit f-name "")]
    f-name))

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

(defn output-seq-map [filename smap selected-k]
  (let [path (output-path (str filename ".csv"))
        head (flatten (->> smap (take 1) (map keys)))
        _ (clean-file path)
        selected (set (->> selected-k ))
        ]
    (if-not (nil? selected-k)
      (do
        ;; write selected header
        (write-csv-line path (keep selected head))
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
    (str prefix path "?" params)
    ;kv-pairs
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
    (name x)
    ))

(defn slash-keys [& xs]
  (keyword (str/join "/" (map #(force-str %) xs))))

(defn slash-flatten-map [m]
  (->> (flatten-hashmap m)
       (map #(map-on-key (fn [x] (apply slash-keys x) ) %))
       (apply merge)
       ))


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

;; load from default config file
;;  i.e. (config :chrome-profile)
;;       a varible chrome-profile is defined
(defmacro config [name & {:keys [config-path]
                          :or {config-path "resources/config.edn"}}]
  (let [var-name (symbol (force-str name))
        key-word (keyword (force-str name))]
    `(def ~var-name (~key-word (load-edn "resources/config.edn")))))


(comment
  (tree-diff a b vcf-count)
  (def a {:a [1 2 3] :b {:bb [22 23] :d 1} :c 2}) (def a {"b" [{:bb ["1" "2" "3"] :c ""}]})
  (def b {:a [1 2] :b {:bb [22] :d 2}  :c 1})
  (tree-diff "abc" "def" vcf-count)
  (tree-diff [1 2 3] [2 3 4] vcf-count)
  (def a {"stuff" {"version" 4}})

  (flatten-hashmap  a)
  (flatten-hashmap  nest-a)
  (tree-diff nest-a nest-b  (fn [a b] (= a b)))
  ;(tree-diff nest-a nest-b  =)
  (map keys
       (flatten-hashmap nest-a))
  (def ip "127.0.0.1")
  (= ip (int-2-ip (ip-2-int ip)))
  ;;
  )

