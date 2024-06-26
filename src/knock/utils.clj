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
   [babashka.process :as proc]
   [babashka.curl :as curl]
   [clojure.zip :as z]
   [clojure.core.async :as async :refer [go go-loop
                                         chan to-chan into
                                         <! >!! >! close!
                                         <!! alt!
                                         thread
                                         thread-call
                                         pipeline-async]]
   [clj-yaml.core :as yaml :refer [parse-string]]
   [clojure.edn :as edn]
   [clojure.walk :as walk]))

(def os-name (System/getProperty "os.name"))
(defn osx?[] (= os-name "Mac OS X"))

(declare force-str force-int cur-time-str ->keyword ->uuid)
(declare split-by tmp-file mock md5-uuid ->abs-path spit-line pp-hashmap!
         file-name ext-name var-meta async-fn tail-f
         )

(defn uuid []
  (java.util.UUID/randomUUID)
  )

;;println but return the element
(defn println! [x]
  (let []
    (println x)
    x)
  )


(defn join-cmd [& cmd]
  (str/join " " (->> cmd (map force-str))))

(defn show-cmd [& cmd]
  (let [s (apply join-cmd cmd)
        _ (println s)] s))

(defn run-cmd [& cmd]
  (sh "sh" "-c" (apply join-cmd cmd))
  )

(defn run-cmd! [& cmd]
  (:out (apply run-cmd cmd))
  )

;; seperator : to split key value -> a hash map
(defn run-cmd!! [& cmd]
  (->>
    (str/split-lines 
      (:out (apply run-cmd cmd))
      )
    (map #(str/split % #":" 2))
    (remove #(= 1 (count %)))
    (map (fn [[k v]]
           (let [kk (-> (re-seq #"\*\s+(.*)" k)
                        first
                        second 
                        )
                 ]
             (if-not (nil? kk)
               {(->keyword (str/replace kk " " "-")) v}
               (if (and (not(empty?  k))
                        (not(empty?  v)))
                 {(->keyword k) v}
                 nil
                 )
               )

             )
           )
         )
    (apply merge)
    )
  )

(defn run-shell [& cmd]
  (proc/shell (apply join-cmd cmd) )
  )

(defn ->stdout! [f] (async-fn #(println %) (tail-f f)))

(defn to-path-cmd [path & cmd]
  (sh "sh" "-c" (apply join-cmd "cd " path "&&" cmd)))

(defn eval-with-timeout [seconds f]
  (let [fut (future (f))
        ret (deref fut (* 1000 seconds) :timed-out)]
    (when (= ret :timed-out)
       (future-cancel fut))
    ret))

(defn timeout-eval [seconds f & args]
  (let [fut (future (apply f args))
        ret (deref fut (* 1000 seconds) :timeout)
        ]
    (when (= ret :timeout)
      (future-cancel fut))
    ret)
    )

(def pp clojure.pprint/pprint)

(defn mpp [& args]
  (binding [*print-meta* true]
    (apply clojure.pprint/pprint args)
    )
  )


(defn do-println [f & args]
  (let [s (apply str (join-cmd args))]
    (println "running with args"
             (if (< 256 (count s))
               (str " too long stored here: " (tmp-file s))
               s))
    (let [res (apply f args)]
      (println res)
      res
      )))

(defn ->str [x]
  (if (string? x)
    x
    (if (keyword? x)
      (name x)
      (str x)
      )))

(defn ->keyword [x]
  (if (keyword? x)
    x
    ;; (keyword ":abc") should be :abc 
    (keyword
     (let [s (->str x)]
       (if (str/starts-with? s ":")
         ;;remove the leading :
         (subs s 1)
         s
         )))))

(def force-str ->str)

(defn num-from [n]
  (iterate inc n))

(defn ->k [n]
  (let [x (force-int n)]
    (if (nil? x)
      nil
      (quot x 1024))))

(defn ->m [n]
  (-> (->k n)
      (quot 1024)))

(defn ->g [n]
  (-> (->m n)
      (quot  1024)))

(defn ->log [& xs]
  (str/join " " (concat [(cur-time-str)] (map force-str xs))))

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

(defn is-or-has [x]
  (if (map? x)
    (let [xs x]
      #(->> xs
            (map (fn [[k v]]
                   (= (get % k) v)
                   ))
            (every? true?)
            )
      )
    (partial = x)
    )
  )


(defn ->>! [f x colls]
  (->> colls
       (f #((is-or-has x) %))
       )
  )

(def filter! (partial ->>! filter))
(def remove! (partial ->>! remove))


(comment
  (every? true?
          [((is-or-has {:k 1 :b 2}) {:k 1 :b 2 :c 3})
           ((is-or-has {:k 1}) {:k 1})
           (empty? (remove! {:k 1} [{:k 1}]))
           (not (empty? (filter! {:k 1} [{:k 1 :facts "abc"} {:k 2} {:k 3}])))
           ]
          )
  )

;;"true if coll contains elm"
(defn in? [coll elm & {:keys [cmpfn]
                       :or   {cmpfn =}}
           ]
  (let [res (some #(cmpfn % elm) coll)]
    (if (nil? res)
      false
      true
      ))
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

(defn fuzzy-rev-in? [coll elm]
  (rev-in? coll elm :cmpfn str/includes?))

;;run f every t seconds
;;  return a channel
;;       call close! on the channel if you would like to stop the timer
(defn timer [f t-sec]
  (let [exit-ch (chan)]
    (go-loop []
      (f)
      (alt!
        (async/timeout (* t-sec 1000)) (recur)
        exit-ch nil
        )
      )
    exit-ch)
  )

(defn ->ch [stream]
  (let [c (chan)]
    (go
      (with-open [reader (clojure.java.io/reader stream)]
        (doseq [ln (line-seq reader)]
          (>! c ln)))
      (close! c))
    c))

(defn tail-f [path]
  (->ch (:out (proc/process "tail -n0 -F " path)))
  )

(defn pipeline-demo [n data f]
  (let [out (chan)
        log (chan 10)
        data (to-chan data)
        task (fn [value channel]
               (go
                 (>! log value)
                 (>! channel (f value))
                 (close! channel)))]

    (go-loop [] (when-some [line (<! log)] (println line) (recur)))
    (pipeline-async n out task data)
    (time (<!! (async/into [] out)))))

(defn async-fn [f c]
  (go-loop [] (when-some [xs (<! c)]
                  (f xs)
                  (recur))))


;; hope not have to require async fns everytime
;; go! is actually go, but wont bother to do require
(defmacro go! [& xs] `(go ~@xs))
(defmacro thread! [& xs] `(thread ~@xs))
(defmacro to-chan! [& xs] `(to-chan ~@xs))


;;TODO local server
(defn go!! [f & xs]
  (let [id (md5-uuid (str/join (map force-str (cons f xs))))]

    {:id id}

    ))



(comment

  (def c (to-chan! [1 2 nil 3]))

  (pipeline-demo 3 #(println %) )

  (async-fn #(println %) c)
  @(chan-worker!! #(doall (println %))
                  (to-chan (range 3)))



  (go!! println 'hi)

  )

;; chan! is actually chan
(defmacro chan! [& xs] `(chan ~@xs))
;;see the trace to avoid doing require
(defmacro ->! [& xs] `(>! ~@xs))
(defmacro <-! [& xs] `(<! ~@xs))
(defmacro ->!! [& xs] `(>!! ~@xs))
(defmacro <-!! [& xs] `(<!! ~@xs))

(defn count! [f & args] (go (count (apply mock f args))))

;; for others perhaps try async/
;(alias 'async 'clojure.core.async)

(def count-down "bb -e '(doseq [x (range (apply + (->> *command-line-args* (map #(Long/parseLong %)))))] (println x) (Thread/sleep 1000))' 3")

(defn count-print [n]
  (->> (range n)
       (map (fn [x]
              (println x)
              (Thread/sleep 1000)))
       count))


;;async run return the hashmap including
;; :proc process information
;; :out the stream
;; see the full with (run! count-down)
(defn run! [& args]
  (let [log (tmp-file (str/join " " args))
        m (assoc
           (proc/process "sh -c" (apply join-cmd args))
           :cmd args
           :log log
           :res (promise))
        {:keys [out proc cmd]} m]
    ;;collecting log
    (async-fn #(spit-line log %) (->ch out))
    m
    ))

;;run after the promise realized means preparation done
(defn run-after! [p & args]
  (let [log                    (tmp-file (str/join " " args))
        m                      {:cmd args
                                :log log
                                :res (promise)
                                ;;a promise when process started
                                ;; deliver an m with :proc point to running process
                                :started (promise)
                                }
        ]
    (thread!
      ;;wait for the preparation promise from outside
      @p
      (let [{:keys [out cmd proc]
             :as cur-proc} (proc/process "sh -c" (apply join-cmd args))]
        ;;collecting stdout to log
        (async-fn #(spit-line log %) (->ch out))
        (deliver (:started m) (-> m (dissoc :started) (assoc :proc proc) ) )
        )
      )
    m
    )
  )



(defn call-context [f & args]
  {:id (str (:name (var-meta f)) "-" (md5-uuid (str/join " " args)))})

(def call-tasks (atom {}))

;;start the function 
(defn- call [f & args]
  (let [{:keys [id]} (apply call-context f args)
        log (join-path "/tmp" (str id ".tmp"))
        res (promise)]
    (thread!
     (binding [*out* (java.io.FileWriter. log)]
       (let [x (apply f args)]
         (deliver res x)
         x)))

    {;; a log file in the /tmp/
     :log log
     :id id
     ;;async return using @ to dref the result 
     :res res}
;;
    ))

;;if not running start the function
;;if running, return the context and progress of running 
;; when running return the status
(defn call! [f & args]
  (let [{:keys [id]} (apply call-context f args)
        t (get @call-tasks id)]
    (if (nil? t)
      (let [t (apply call f args)]
        (swap! call-tasks merge {id t})
        (-> t
            (dissoc :res)
            (assoc :status "running"))
        )
      ;;
      (let [{:keys [res id]} t]
        (if (realized? res)
          (do
            ;;remove the task record for not blocking next call
            (swap! call-tasks dissoc id)
            (assoc t :res @res))
          (-> t
              (dissoc :res)
              (assoc :status "running")))))))

;;call f on each element of coll
(defn worker! [f coll]
  (let [p (promise)]
    ;;(println "start worker to consume" (count coll))
    (thread!
      (let [xs (->> coll
                    (map f)
                    (apply list))]
        (deliver p xs)))
    p
    ))

(defn partition!! [n coll]
  (if (= 0 n)
    (->> coll (map (fn [x] [x])))
    (partition-all n coll)
    )
  )

;;wait util all worker completed
(defn pipeline!! [n f coll]
  (let [c-count (count coll)
        per     (quot c-count n)
        xs (partition!! per coll)
        ;_ (println (count xs))
        ps      (pmap #(worker! f %) xs)
        ]
    (thread!

      (filter realized? ps)

      )
    ;;TODO fix not in flatten way
    (pmap (fn [x] @x) ps)
    )
  )

(comment
  (partition-all 3 (range 10))

  ;;3 worker. each print wi
  (pipeline!! 6 #(do (Thread/sleep 100)
                     (println %)
                     (Thread/sleep 100)
                     ) (range 3))

  )

(comment
  (def x
    (call! count-print 10)
    )

  (def x (proc/shell count-down 100))
  ;;
  )

(def count-down! (partial run! count-down))

;;every second show run! process background current log 
(defn stat-log! [{:keys [out proc cmd log res]
                  :as m}]
  ;;deliver when finished
  (thread!
   (while (.isAlive proc)
     (println "cur:" (:out (run-cmd "tail -n 1 " log)))
     (Thread/sleep 1000)
     (deliver res m)
     )
   ))

(defn cur-line [f]
  (:out (run-cmd "tail -n 1 " f)))

(defn alive? [p]
  (.isAlive (:proc p)))

(defn some-alive? [xs]
  (some #(.isAlive (:proc %)) xs)
  )

;;concurrent run and collect result
(defn cc-run [xs]
  (let []
    (thread!
     (while (some-alive? xs)
       (println (str/join "" (repeat 42 "-")))
       (Thread/sleep 1000)
       (pp-hashmap! (->> xs (map #(assoc % :cur-log (cur-line (:log %))))) :log :cur-log))
     ;;completed
     (doseq [x xs] (deliver (:res x) x)))
    ;;block and wait
    (doseq [x xs]
      @(:res x)
      )
    ;;do the something to finalize maybe

    ))

(comment
  (def args [count-down])

  (let [xs (->> (range 3) (map #(run!
                                  count-down
                                  %)))]
    (cc-run xs)
    )

  (run! count-down 1)

  (def p (promise))

  (thread! (Thread/sleep 3)
           (deliver p nil)
           )

  (def t (run-after! p count-down 1))

  @(:started t)
  ;;
  )

(defn range! [n]
  (range 1 (+ 1 n))
  )


(defn >0-and-lt? [n x]
  (and
    (> x 0)
    (< x n)
    )
  )

;; TODO meta

;; get all the command string of shell
(defmacro make-shell-fn [name & default-args]
  (let [fname (symbol (force-str name))
        args  (symbol "args")]
    `(defn ~fname [& ~args]
       (str/trim-newline
         (:out
          (apply (partial run-cmd ~name) ~@default-args ~args))))))


(make-shell-fn "basename")
(make-shell-fn "openssl")
(make-shell-fn "base64")
(make-shell-fn :dirname)
(make-shell-fn "curl")
(make-shell-fn "grep")
(make-shell-fn :ls )
(make-shell-fn :readlink)
(make-shell-fn :touch)


(def work-dir
  (let [path (dirname (System/getProperty "babashka.config"))]
    (if (empty? path)
      "./"
      path
      )))


(defn base64-encode [s]
  (str/trim-newline
   (:out (run-cmd :echo s "| base64"))))

(defn base64-decode [s]
  (str/trim-newline
   (:out (run-cmd :echo s "| base64 -d"))))


(defn enc [password s]
  (let [path (tmp-file s)
        out (str/trim (run-cmd! "openssl enc -aes128 -pbkdf2 -a -e -k" password "-in" path))
        ]
    (fs/delete-if-exists path)
    out
    )
  )

(defn dec-by [password s]
  (let [path (tmp-file s)
        out (run-cmd! :openssl  "enc -aes128 -pbkdf2 -a -d -k" password "-in" path)
        ]
    (fs/delete-if-exists path)
    (apply str (drop-last out))
    )
  )

(comment
  (dec "PASSWORD"
    (enc "PASSWORD" "abc\n")
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tmux related operation
;;    should have make TUI automation easier 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tmux [s]
  (let [id (->uuid s)]
    (if
     (str/includes? "tmux list-sessions" uuid)
      (run-shell "tmux new-session -s " id)
      (run-shell "tmux attach -t " id))
    id))


(defn send-text [id & xs]
  (run-cmd "tmux send-keys -t" id "'" (str/join " " xs) "'"))

(defn send-keys [id & xs]
  (apply run-cmd "tmux send-keys -t" id xs))

(defn clear [id]
  (send-keys id "c-c")
  (send-keys id "c-l"))




(defn file-ext [s]
  (last (str/split s #"\."))
  )

(defn file-name [s]
  (let [xs (str/split (basename s) #"\.")]
    (str/join "."
              (if (= 1 (count xs))
                xs
                (drop-last xs)))))

(defn re-escape [s]
  (str/escape s {\. "\\."

                 \[ "\\["
                 \] "\\]"
                 \{ "\\{"
                 \} "\\}"
                 })
  )

(defn ->re [s]
  (if (string? s)
    (re-pattern s)
    s
    ))

(defn split-by [s & re]
  (let [c (->re (if (nil? re)
                  " "
                  (first re)))]
    (if (< (count s) 2)
      s
      (str/split s c))))


(defn split-pairs [s re & keys]
  (apply hash-map (interleave keys (str/split s re))))

;; shell text ouput -> hash-map
;;   with header line as keys
;;   each line a hash-map
(defn text-cols->hashmap
  ([s]
   (text-cols->hashmap s #"\s\s+"))
  ([s separator]
   (let [xs (str/split-lines s)
         head (first xs)
         ks (map keyword (str/split head separator))]
     (->> (rest xs)
          (map (fn [x] (zipmap ks (str/split x separator))))
          ;;
          ))
   ))


;;trim a string's the starting chars, until match sub
(defn trim-to [s sub]
  (if (str/starts-with? s sub)
    s
    (trim-to (apply str (rest s)) sub)
    ))

(defn trim-between [s from to]
  (let [[before _ after ] (rest (first (re-seq (re-pattern (format "(.*)(%s.*%s)(.*)" from to)) s )))]
    ;;remove [from .* to]
    ;;return before and after
    (str before after)
    )
  )


(defn crt->pem [path]
  (openssl "x509 -inform DER -in" path)
  )


(defn cert->chain [path]
  (openssl "x509 -in" path "-text | grep 'CA Issuers - URI' |awk -FURI: '{print $2}' | xargs curl -s | openssl x509 -inform DER")
  )

(defn dns [path]
  (let [raw (split-by
              (run-cmd! :openssl "x509 -noout -text -in " path "|grep DNS")
              ",")
        xs  (->> raw (mapcat #(re-seq #".*DNS:(.*)" %))
                (map second)
                )
        ]
    (if (some true? (map #(str/starts-with? % "*" ) xs))
      (->> xs (filter #(str/starts-with? % "*")) first)
      (first xs)
      )
    )
  )


;;pure sequentially list a directory
(defn ls-f1 [path]
  (drop 2
        (str/split-lines
         (:out
          (run-cmd "ls" "-f1" path)))))

(defn digit? [s]
  (every? #(Character/isDigit %) s))

(defn parse-int [s]
  (if (str/blank? s) 0 (Long/parseLong (re-find #"\A-?\d+" s))))

(defn force-int [s]
  (if (string? s) (parse-int s) s)
  )

(def int! force-int)


(defmacro bb-version-deps-fn [version]
  (let [ok? (< -1 (compare (System/getProperty "babashka.version") version))]
    (when ok? 
      `(defn show-members[c]
         (let [detail (:members (clojure.reflect/reflect c))]
           (clojure.pprint/print-table [:name :type :flags] (sort-by :name detail))
        c))
      )
    )
  )

(bb-version-deps-fn "1.3.187")
(comment

  (show-members (java.io.File. "foo"))

  )

(defn ->int[s]
  (re-seq #"\d+" s)
  )

(defn ->int! [s]
  (first (->int s)))


(defn parse-float [s]
  (if (string? s)
    (if (= s "+Inf")
      ##Inf
      (Float/parseFloat s))
    s))

(defn force-float [s]
  (if (nil? s)
    0
    (if (string? s)
      (parse-float s)
      (float s))))

(defn cur-time-str
  ([]
   (cur-time-str "yyyy-MM-dd hh:mm:ss"))
  ([fmt]
   (.format (java.text.SimpleDateFormat. fmt) (java.util.Date.)))
  ([fmt zone]
   (.format
    (doto (java.text.SimpleDateFormat. fmt)
      (.setTimeZone
       (java.util.TimeZone/getTimeZone "GMT")))
    (java.util.Date.))))

(defn time-tag []
  (cur-time-str "yyyy-MM-dd..hh-mm-ss")
  )

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

(defn ->date-format [s]
  (java.text.SimpleDateFormat. s)
  )

(def time-fmts (->> ["MMM dd HH:mm:ss yyyy"
                     "yyyy-MM-dd HH:mm:ss"
                     "yyyy-MM-dd'T'HH:mm:ssXXX" ;2023-10-02T16:56:28+08:00
                     "yyyy-MM-dd'T'HH:mm:ss"
                     ]
                    (map #(java.text.SimpleDateFormat. %))))

(defn ->inst [s & {:keys [to-trim]
                           :or {to-trim ".*expire date: "}}]
  (->> time-fmts
       (map #(try (.parse % (str/replace-first s (re-pattern to-trim) ""))
                  (catch Exception e nil)))
       (remove nil?)
       (first)))

(defn ms [t]
  (if (nil? t)
    t
    (inst-ms t)))

(defn ts [t]
  (if (nil? t)
    t
    (quot (ms t) 1000)))

(defn +days [t n]
  (from-timestamp (+ (ts t) (* n 86400))))

(defn -days [t n]
  (from-timestamp
    (- (ts t) (* n 86400))))

(defn cur-ts-13 []
  (inst-ms (java.util.Date.)))

(defn cur-ts []
  (quot (cur-ts-13) 1000))

(comment
  (ts
    (->inst "*  expire date: Jul 24 23:30:12 2023 GMT")
   )
  )


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

(def days-from-now (partial days (java.util.Date.)))
(defn days-to-now[from] (days from (java.util.Date.)))
(defn to-now-seconds [t]
  (if (nil? t)
    t
    (if (inst? t)
      (- (ts t) (cur-ts))
      (- (ts (->inst t)) (cur-ts)))))

(defn to-now-hours [t]
  (/ 
    (float (to-now-seconds t))
    3600
    ))

(defn from-now-seconds [t]
  (- (to-now-seconds t))
  )

(defn from-now-hours [t]
  (- (to-now-hours t))
  )

(defn not-expire? [t]
  (if (nil? t) false
      (< 0
         (to-now-seconds t))))

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

(defn days-left [expire-date]
  (days-from-now
    (if (string? expire-date)
      (->inst expire-date :to-trim " " )
      expire-date
      )))

;;define an assoc version of function
(defmacro ++ [f & args]
  (let [name+    (symbol (str f "++"))
        ;;TODO when find the way of got meta of new added function
                                        ;{:keys[arglists]} (var-meta f)
        args     (apply vector args)
        name-key (->keyword(str f))
        ]
    `(defn ~name+ [{:keys ~args
                    :as   m}
                   ]
       (assoc m ~name-key (apply ~f ~args))
       )
    )
  )

;;merge ++
(defmacro m++ [f & args]
  (let [name+    (symbol (str f "++"))
        ;;TODO when find the way of got meta of new added function
                                        ;{:keys[arglists]} (var-meta f)
        args     (apply vector args)
        name-key (->keyword(str f))
        ]
    `(defn ~name+ [{:keys ~args
                    :as   m}]
       (merge m (apply ~f ~args))
       )
    )
  )

(++ days-left expire-date)



(def j json/generate-string)
(def e clojure.edn/read-string)

(defn jstr->edn [s] (json/parse-string s true))
(def jstr-to-edn jstr->edn)

;;ignore failed
(defn jstr->edn! [s]
  (try
    (jstr->edn s)
    (catch Exception e
      nil)
    )
  )


(defn object? [x]
  (instance? clojure.lang.IFn x))

;; string of json, edn, parsed to be edn 
(defn ->edn [s]
  (let [x (jstr->edn! s)]
    (if (nil? x)
      (try 
        (e s)
        (catch Exception e
          nil
          )
        )
      x
      )
    )
  )

(defn ->hash-map [k-xs v-xs]
  (->> v-xs
       (map #(zipmap k-xs %))
       ))

(defn name-xs [k xs]
  (->> xs
       (map #(if (sequential? k)
               (zipmap k %)
                 ;;compliance with zipmap
               (zipmap [k] [%])
                 )
               )
       )
  )

;;slurp but using line-seq 
(defn slurp-lines [f]
  (line-seq (clojure.java.io/reader f)))

;;slurp but remove comment, start with #
(defn slurp-lines! [f]
  (->> (slurp-lines f)
       (remove #(str/starts-with? % "#" ))
       )
  )


;;edn or json
(defn slurp-ej-line [f]
  (->> (slurp-lines f)
       (map ->edn)
       ;;ignore only string 
       (remove symbol?)
       (remove nil?)
       )
  )

(defn slurp-ej-line! [f]
  (->> (slurp-lines! f)
       (map ->edn)
       (remove symbol?)
       (remove nil?)
       )
  )

(comment 
  (->hash-map ["a" "b" "c"] [[1 2 3] [4 5 6]])

  (name-xs :ip ["127.0.0.1" "192.168.1.1"] )
  ;;?support ["127.0.0.1" 1 "192.183.1.1" 2]  
  (name-xs [:ip :count] [["127.0.0.1" 1] ["192.168.1.1" 2]] )

  (->edn "{:a :b}c")
  (->edn "{\"a\": 1}")

  )


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
  (let [req (if (nil? body)
              {:headers headers}
              (assoc {:headers headers} :body (j body)))
        ;;_ (println req)
        res (try (method url req) (catch Exception e (ex-data e)))]
    (-> res
        (assoc :body (try
                        (jstr-to-edn (:body res))
                        ;;can't convert to edn, just return
                        (catch Exception e (:body res))))
        ;;remove :process which will cause No reader function for tag object
        (dissoc :process)
        )
    ;
    ))


(def curl-get (partial curl-any curl/get))
(def curl-post (partial curl-any curl/post))


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

(defn ->ip [s] (first (extract-ip s)))
(defn ->ips [s] (extract-ip s))
;; a direct ip of this network.
;;  if running on a server, it happen to be the server's ip
;;  when running on a machine behind NAT, it will be the ip of its network
(defn my-ip []
  (str/trim (:out (run-cmd "curl -4 ip.sb")))
  )

(defn ip? [s]
  (not (empty? (extract-ip s)))
  )

(defn ->dash [ip]
  (str/replace ip #"\." "-"))

(defn remove-last-char [char domain]
  (if (= (last domain) char)
    (apply str (drop-last domain))
    domain)
  )

(def remove-last-dot (partial remove-last-char \.))

(defn ->domain [url]
(let [x (second
          (first
            (re-seq #".*:\/\/(.*)[\:|\/]"
                    (if (str/ends-with? url "/")
                      url
                      (str url "/")))))]
    (if (nil? x)
      x
      (if (str/includes? x ":")
        (first (str/split x #":" ))
        x
        ))))

;;url or domain
(defn domain->ip [x]
  (let [domain (->domain x)
        domain (if (nil? domain)
                x
                domain)]
    (.getHostAddress (java.net.InetAddress/getByName domain))
    ))

(defn domain? [x]
  (not (nil?
        (re-seq #".*\.?.*\..*" x)
        )))

(comment
  (domain? "域名")
  (domain? "x.com")
  (domain->ip "https://www.x.com:1663")
  (->domain "https://www.x.com:1663/")
  (->domain "https://www.x.com:1663")
  (->domain "https://www.x.com/")
  (->domain "https://www.x.com")
  (->domain "www.x.com"))

(defn dig-domain-ips [domain]
  (let [x (->domain domain)
        domain (if (nil? x)
                 domain
                 x)]

    (extract-ip
     (:out (run-cmd "timeout 3 curl" "-s" "-v" (str "https://" domain ":15986 2>&1 | grep Trying")))))
  ;;
  )


;; File creating. cleaning. path assembling
(defn clean-file [f-name]
  (let [mkdir (clojure.java.io/make-parents f-name)
        cleared (spit f-name "")]
    f-name))


(defn spit-line [f s]
  (let [s (if (string? s ) s (str s))
        line (if (str/ends-with? s "\n")
               s
               (str s "\n"))]
    (spit f line :append true)
    )
  )

(defn slurp-yaml [f]
  (yaml/parse-string (slurp f)))

;;multiple yaml
(defn slurp-yaml! [f]
  )

(defn ->yaml [m]
  (yaml/generate-string m :dumper-options {:flow-style :block}))

;;- replaced to be two space
;;{} replaced to be 
(defn ->yaml! [m]
  (->
    (yaml/generate-string m :dumper-options {:flow-style :block})
    (str/replace " {}" "")
    (str/replace "- " "  "))
  )


(defn str-or-file [x]
  (if (fs/exists? x)
     (slurp x)
     x
     ))

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
   (format "%s/resources/conf/%s.edn" work-dir (path-str-clean name))))

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

(defn pp-str [x]
  (with-out-str
    (clojure.pprint/pprint x)))

(defn pp-spit [path x & opts]
  (apply spit path (pp-str x)
        opts)
  )


;;make a function 
;; if a keyword-fn, no suffix
;; if a [keyword-fn ".txt"] (keyword-fn m).txt
(defn suffix-decorate [f]
  (let [[f suffix] (if (sequential? f) f [f])]
    (if (nil? suffix)
      f
      (comp #(str % suffix) f))))

;;csv way of hashmap -> str
(defn hashmap->str [sep xs & ks]
  (let [xs (if (sequential? xs) xs [xs])
        val-fn (if (empty? ks)
                 vals
                 (fn [m] (map #((suffix-decorate %) m) ks)))
        header (if (empty? ks)
                 (keys (first xs))
                 (map #(if (sequential? %) (first %) %) ks))]
    (str/join "\n"
              (cons
                ;;header
                ;;compliance with text-cols->hashmap
               (str/join sep (map force-str header))
                ;;lines
               (->> xs
                    (map #(str/join sep (val-fn %))))))
    ;;
    ))

(def pp-hashmap (partial hashmap->str "\t\t"))
(def csv-hashmap (partial hashmap->str ","))
;;! means println the result
(def pp-hashmap! (comp println pp-hashmap))
(def csv-hashmap! (comp println csv-hashmap))

(defn frequencies->str [xs]
  (pp-hashmap
    (->> 
      (frequencies xs)
      (map (fn [[k v]]
              {:name k
               :count v}
             ))))
  )

(frequencies->str ["a" "b" "a"])
(declare slash-flatten-map)

;;layered way of hashmap -> str
(defn lp-hashmap [m]
  (->>
   (slash-flatten-map m)
   (map (fn [[k v]]
          (str
           (str/join "\n" [(force-str k) (str "\t" (force-str v))])
           "\n"
           "\n")))

   (apply str)
   ;;
   ))


(defn spit-xs [path xs & opts]
  (let [_ (clojure.java.io/make-parents path)]
    (apply spit path
           ;;an empty line in the end of the fie
           (str (str/join "\n" xs) "\n")
           opts)))

(defn spit-xs! [path xs & opts]
  (let [res (apply spit-xs path xs opts)
        ]
    (when-not (nil? res)
      (println "writing failed " path " count xs: " (count xs)))
    path
    )
  )

;;load conf from resources/conf path
(defn load-conf [name]
  (load-edn (conf-path-edn name))
  )

(defn agora-conf []
  (load-conf "agora")
  )

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

(defn output-seq-map [filename xs & selected-k]
  (let [path (output-path (str filename ".csv"))
        head (flatten (->> xs (take 1) (map keys)))
        _ (clean-file path)
        selected (set (->> selected-k))
        head-defaults (zipmap head (repeat (count head) ""))
        selected-defaults (zipmap selected-k (repeat (count selected-k) ""))]
    (if-not (nil? selected-k)
      (do
        ;; write selected header
        (write-csv-line path selected-k)
        ;; write selected values
        (->>
         xs
         (map #(select-keys % selected-k))
         ;;in case of missing fields set to empty
         ;;(map #(merge selected-defaults %))
         (map (apply juxt selected-k))
         (map #(write-csv-line path %))))

      (do
        ;; write header
        (write-csv-line path head)
        ;; write values
        (->> xs
             (map #(merge head-defaults %))
             ;(map #(println (when (= (type %) 'java.lang.string) (println %))))
             (map vals)
             (map #(write-csv-line path %))
             )))))

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
       (partition-all 2)
       (map #(apply str "0x" %))
       (map read-string)
       (str/join ".")))

;; n max 255
(defn ip-offset [s n]
  (let [xs (->>
            (Integer/toHexString (ip-2-int s))
            (partition-all 2)
            (map #(apply str "0x" %))
            (map read-string))]
    (str/join "."
              (conj (vec (take 3 xs))
                    (+ n (last xs))))))

(defn exp [x n]
  (loop [acc 1 n n]
    (if (zero? n) acc
        (recur (* x acc) (dec n)))))

;;127.0.0.1/26
(defn range->ips [ip-range]
  (let [[start mask] (str/split ip-range #"/")
        [a b c d] (str/split start #"\.")
        dn (force-int d)
        count (exp 2 (- 32 (force-int 27)))
                 ]
    (->>
     (range count)
     (map #(+ dn %))
     (map #(str/join "." [a b c (force-str %)]))
     )
    ;;
    ))

(comment
  (range->ips "98.98.205.160/27")
  )


(defn ip+n [n s]
  (->>
   (str/split s #"/")
   (first)
   (ip-2-int)
   (+ n)
   (int-2-ip)
   )
  )




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

(declare slash-flatten-map)

(defn cherry-rename [m & ks]
  (->> ks
       (map #(identity
              {% (first (vals (slash-flatten-map (cherry-pick m % ))))}))
       (apply merge)
       ;;
       ))

(defn rename [m k new-k]
  (if (nil? (k m))
    ;;already nil. do nothing
    m
    (dissoc (assoc m new-k (k m)) k))
  )

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
         ;;two sets of key path have common field 
         (remove #(empty? (clojure.set/intersection hs (apply hash-set (flatten (keys %))))))
         ;;(map #(apply hash-set (flatten (keys %))))
         ;;(map #(map-on-key (fn [xs] (apply slash-keys xs))))
         ;;
         )))

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
       (apply merge)
       ))


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
                      (->keyword %)
                      )
                          %)
                 )
            repeat)
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

;;parse a byte from \x encoded string
;;  could have a better way on performance
(defn x-first-byte [s]
  (let [hex (second (re-find #"\\x(..)" (apply str (take 4 s))))]
    (if (nil? hex)
      (if (empty? s) [nil nil] [(byte (first s)) (drop 1 s)])
      [(Integer/parseInt hex 16) (drop 4 s)])))

;;\x encoded string
(defn x->utf8 [s]
  (let [u (loop [s s res []]
            (let [[c xs] (x-first-byte s)]
              (if (nil? s)
                res
                (recur xs (if (nil? c)
                            res
                            (conj res c))))))]
    (String. (byte-array u))))

(defn url-kv->hashmap [s]
  (let [[k v] (str/split s #"=")]
    {(->keyword k) v}))

(defn parse-uri [s]
  (let [raw (java.net.URLDecoder/decode s)
        [path params] (str/split raw #"\?")]
    [ ;;first part path
     (remove empty? (str/split path #"/"))
     ;;second part kv-params
     (if (nil? params)
       nil
       (->> (str/split params #"&")
            (map url-kv->hashmap)
            (apply merge)))]
    ))

(defn url-encode [v] (java.net.URLEncoder/encode v))
(defn url-decode [v] (java.net.URLDecoder/decode v))

(defn make-url [prefix path & kv]
  (let [kv-pairs (flatten
                  (map #(if (map? %) (flatten (seq %)) %) kv))
        params (->> kv-pairs
                    (partition 2)
                    (map #(str (name (first %))
                               "="
                               ;;make sure the space and other characters are encoded as url
                               (url-encode
                                (if (keyword? (second %))
                                  (str (name (second %)))
                                  (str (second %))))))

                    (str/join "&")
                    (apply str))]

    (if (empty? params)
      (str prefix (force-str path))
      (str prefix (force-str path) "?" params))))

(defn ->k=v [xs & {:keys [separator prefix]
                   :or {separator "="
                        prefix ""}}]
  (->> xs
       (map #(str prefix (str/join separator (map force-str %))))
       ;(apply str)
       )
  )

;REPL

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
       ))


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


(defn name-a-key [m from to & keep-original?]
  (if (empty? keep-original?)
    (dissoc (assoc m to (from m)) from)
    (assoc m to (from m))))
(defn f-on-a-key [m k f]
  (assoc m k (f (k m))))


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
(defmacro config [name & {:keys [config-path default config-m]
                          :or {config-path (join-path work-dir "resources/config.edn")}}]
  (let [var-name (symbol (force-str name))
        key-word (keyword (force-str name))
        m (if (nil? config-m) (load-edn config-path) config-m)
        value (key-word m)]
    `(def ~var-name
       (if (nil? ~value)
         ~default
         ~value))))

;;config multiple from the same file
(defmacro configs [names & {:keys [config-path]
                            :or {config-path (join-path work-dir "resources/config.edn")}}]
  (let [m (load-edn config-path)]
    `(do
       ~@(for [x names]
           `(config ~x :config-m ~m)))))

(comment
  (configs [:es :rte])
  (config :a :config-m (load-edn "resources/config.edn"))
  )


;; load from shell environment, accept both keyword and string 
;;   def the same name variable
;;    i.e. (env :OPENAI_API_KEY)
(defmacro env [name]
  (let [var-name (symbol (force-str name))
        s (force-str name)
        ]
    `(def ~var-name (System/getenv ~s))
    )
  )


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


(defn fn->>ns []
  (->>
   (all-ns)
   (remove #(fuzzy-rev-in? ["clojure" "babashka" "rewrite-clj" "hiccup2" "etaoin" "org.httpkit" "clj-http" "user" "slingshot" "sci.core" "clj-yaml" "cheshire.core" "selmer"]
                           (str %)))
   (map ns-map)
   (apply merge)
   ;(map-on-val var-get)
   ;;
   ))

(def all-ns-map (atom {}))
(defn get-all-ns-map []
  (if (empty? @all-ns-map)
    (swap! all-ns-map merge (fn->>ns))
    @all-ns-map
    )
  )


;;surely have issue of performance
;;from 
(defn var-meta [f]
  (meta (second (first (filter #(and (var? (second %)) (= f (var-get (second %))))
                               ;(ns-map *ns*)
                               ;(fn->>ns)
                               (get-all-ns-map)
                               )
                       ))))



(defn x-or-xs-fn [x f]
  (if (sequential? x)
    (->> x (map f))
    (f x)))

(defn assoc-time [x-or-xs t & {:keys [key]
                               :or {key :update-time}}]
  (x-or-xs-fn x-or-xs
              (fn [x]
                (if (map? x)
                  (if (contains? x key)
                    x
                    (assoc x key t))
                  x))))

(defn assoc-cur-time [x-or-xs & {:keys [key]
                                 :or {key :update-time}}]
  (assoc-time x-or-xs (cur-time-str)))


(comment
  (jstr->edn "{}")

  (assoc-time (repeat 3 {}) (cur-time-str))
  (assoc-cur-time (repeat 3 {}))
  (assoc-time {} (cur-time-str))
  (assoc-time {} (cur-time-str) :key :update-time)
  ;;
  )


(defn- mock-context [f & args]
  (try
    (let [fm (var-meta f)
          ns-path (str/replace (ns-name (:ns fm)) #"\." "/")
          dir (str work-dir "/resources/mock/" ns-path "/")
          path (if (empty? args)
                 (str dir (:name fm) ".edn")
               ;;(str dir (:name fm) "/" (str/join "-" (map force-str args)) ".edn")
                 (str dir (:name fm) "/"
                      (if (or (str/includes? (str args) ":")
                              (< 200 (count (str args))))
                        (md5-uuid (str/join " " (map force-str args)))
                        (str/join "-" (map force-str args))
                        )
                      ".edn"
                      )
                 )
          t (cur-time-str "yyyy-MM-dd-hh:mm:ss.SSS")
          tmp-path (str path "." t)]
      {:fm fm
       :ns-path ns-path
       :dir dir
       :path path
       :tmp-path tmp-path})
    (catch Exception e
      (throw (Exception.
              (str e "need ref function: "
                   (:name (var-meta f))
                   " in current namesapce")))
                  ;;
      )))

(def mock? mock-context)

(defn- do-mock [force? f & args]
  (let [{:keys [fm ns-path dir path tmp-path t]} (apply mock-context f args)
        _ (clojure.java.io/make-parents path)
        res (if (and (not force?) (fs/exists? path))
      ;;load mock data from local path
              (try
                (load-edn path)
        ;; when failed try call function again and cache
                (catch Exception e
                  (let [res (apply f args)
                        _ (println "failed load-edn" path "check the reader of this edn"
                                   e)
                        _ (fs/delete-if-exists path)]
                    ;;problem: this one is not a symbol link
                    (binding [*print-meta* true]
                      (spit path
                            (pr-str
                              (if (instance? clojure.lang.IFn res)
                                (with-meta res {;:update-time (cur-time-str)
                                                :ts (cur-ts)})
                                res)
                              ))))))
              (let [res (apply f args)
                    _ (clean-file tmp-path)]
                (binding [*print-meta* true]
                  (spit tmp-path
                        (pr-str
                          (if (instance? clojure.lang.IFn res)
                            (with-meta res {;:update-time (cur-time-str)
                                            :ts (cur-ts)})
                            res)
                          )))
                (fs/delete-if-exists path)
                (fs/create-sym-link (fs/absolutize path) (fs/absolutize tmp-path))
                res)
      ;;
              )]
    ;(assoc-time res (str (fuzzy-parse-time (str (fs/last-modified-time path)))))
    res))

;;for a slow return function.
;;  mock will run once save the result to local edn file
;;  the next call of mock will read local file
;;call mock-clean to clean local file
(defn mock [f & args]
  ;;not force
  (apply do-mock false f args)
  )

;;force do query and update mock cache
(defn mock-update [f & args]
  (apply do-mock true f args)
  )

(def mock! mock-update)

(defn mock-clean [f & args]
  (let [{:keys [fm ns-path dir path]} (apply mock-context f args)]
    (when (fs/exists? path)
      (fs/delete path))))


(defn mock-path [f & args]
  (let [{:keys [fm ns-path dir path tmp-path]} (apply mock-context f args)]
    path))

(defn mock-exists? [f & args]
  (fs/exists?
    (apply mock-path f args)
    )
  )

(defn recent-files [hours-from-now root pattern]
  (->>
   (fs/glob root pattern)
   (filter (comp #(< % hours-from-now) from-now-hours ->inst str fs/last-modified-time))))

;;doing laundry
;; clean all not linked cache file
(defn mock-laundry []
  (let [dir (str work-dir "/resources/mock/")]
    ;(fs/walk-file-tree dir  )
    (->> (fs/glob dir "**/*.edn.*")
         (map str)
         (map (fn [x]
                (let [d (dirname x)
                      link (second (first (re-seq #"(.*.edn).(.*)" x)))
                      link-to (->abs-path link)
                      ]
                  (when-not (= x link-to)
                    (println "deleting" x "according to link" link-to)
                    (fs/delete-if-exists x)
                    )
                  ))))))

;;n times per hour
(defn mock-with-rate [n f & args]
  (let [{:keys [fm ns-path dir path]}
        (apply mock-context f args)
        d (dirname path)
        file (basename path)
        recent-call-count  (count
                            (recent-files 1.0 d (str file ".*")))]
    (if (< n recent-call-count)
      (throw (Exception. (str (:name fm) " mock call over rate : " n " times per hour")))
      (apply mock-update f args))))


(defn measure [f & args]
  (let [start (cur-ts-13)
        res (apply f args)
        end (cur-ts-13)
        cost-ms (- end start)]
    (with-meta res {:cost-ts-1 (quot cost-ms 1000.0)
                    })))

(comment
  (mock-with-rate 3 run-cmd "ls")

  (mock-update run-cmd "ls")
;;
  )



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
  (->inst
   (curl "-vI" "--resolve" (str domain ":" port ":" ip)
         (str "https://" domain ":" port) "2>&1" "| grep 'expire date' " "| cut -d: -f2- | xargs")))
(defn precision
  ([x & {:keys [keep-digit] :or {keep-digit 2}}]
   (format (str "%." keep-digit "f")
           (parse-float x)
           )))

(defn percentage
  ([total numerator & {:keys [keep-digit] :or {keep-digit 2}}]
   (precision
     (* 100 
        (/  numerator (float total))) :keep-digit keep-digit)
   )
;;
  )



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

(defn cart
  ([] '())
  ([l1] (map list l1))
  ([l1 l2]
   (map (fn [x]
          (map (fn [y]
                 (list x y))
               l2)
            )
        l1)
    )
)

(defn cartesian-product [& lists]
  (reduce cart lists))

(defn flat-cartesian-product [coll & xs]
  (let [x (first xs)]
    (if (nil? x)
      coll
      ;(flat-cartesian-product (flatten (cart coll x))
      ;                        (rest xs))
      (let [cur-coll  (partition (+ 1 (if (sequential? (first coll))
                                           (count (first coll))
                                           1
                                           ))
                                 (flatten (cart coll x)))]
        (if (nil? (rest xs))
          cur-coll
          (->>
           (apply flat-cartesian-product cur-coll (rest xs)))
          ;;
          )))))


(defn bb-tasks []
  (let [tasks (-> (run-cmd :bb :tasks)
                  :out
                  str/split-lines)]
    (->> tasks
         (map str/trim))
    )
  )


(defn auto-cache-by-expire-time [key f]
  (let [cur (mock f)
        ;;depends on fuzzy-parse-time. add new format to support. when not ok
        t (->inst (get cur key))]
    (if (not-expire? t)
      cur
      (do (mock-clean f)
          (mock f)))))


(def sentence "The quick brown fox jumps over a lazy dog")
(def words (split-by sentence #" "))
(def a-name (str/join "-" words))
(def a-json-file (str/join "." [a-name "json"]))

(defn divisible? [num div]
  (= 0 (mod num div)))


;(take 10 (seive (iterate inc 2)))
;seive prime number in streaming way
(defn seive [xs]
  (let [x (first xs)]
    (cons x
          (lazy-seq
            (seive (->> (rest xs)
                        (remove #(divisible? % x))))))))
;(factorial 3)
(defn factorial
  ([n] (factorial 1 1 n))
  ([product counter n]
   (if (< n counter)
     product
     (factorial (* counter product)
                  (+ counter 1)
                  n
                  )
     )
   ))


(defn pid-path [s]
  (str "/tmp/" s ".pid"))
(defn pid-running? [s]
  (fs/exists? (pid-path s) )
  )
;;a temporary file will be deleted when process exit
(defn pid-file [s]
  (let [path (pid-path s)]
    ;;exist means exit. ignore multiple times create
    (when-not (pid-running? s)
      (fs/create-file path))
    (fs/delete-on-exit path)
    ;;indeed work, may try with a delay
                                        ;(Thread/sleep 1000)
    ))

(defn md5-uuid [s]
  (let [md5 (java.security.MessageDigest/getInstance "MD5")
        encoder (java.util.Base64/getEncoder)
        bytes (.digest md5 (.getBytes s))
        bb (java.nio.ByteBuffer/wrap bytes)
        ;;hex-string (apply str (map #(format "%02x" %) bytes))
        ]
    (java.util.UUID. (.getLong bb) (.getLong bb))))

(defn ->uuid [x]
  (if (uuid? x)
    x
    (md5-uuid (->str x))))

;;do spit, but return the file path
(defn spit! [f content & options]
  (let [_ (apply spit f content options)]
    f
    )
  )

;;use (parital you-actual-function) to suppress evaluation when the cache already exist
(defn tmp-file [s-or-fn & {:keys [dir uuid ext]
                           :or {dir "/tmp/"
                                ext ".tmp"}}]
  (let [f (str dir
               (if (nil? uuid)
                 ;;no uid md5 the string
                 (md5-uuid s-or-fn)
                 (if (uuid? uuid)
                   uuid
                   ;;not a formal uuid. md5 the variable
                   (let [real-uuid (md5-uuid uuid)]
                     (pp-spit (join-path dir "index-uuids.edn") {:orig uuid :uuid real-uuid})
                     real-uuid)))

               ext)]
    (if (and (fs/exists? f) (< 2 (fs/size f)))
      f
      (let []
       (spit f
             (if (fn? s-or-fn)
                 (s-or-fn)
                 s-or-fn)
             )
       (spit f "\n" :append true)
       f
       )
      )))

(defn path->id [path]
  (file-name (basename path)))

;;appoint fields to-search
;;appoint field(s) to choose
;; multiple fields, return the hashmap of selected fields
;; if only one field, just return the value
(defn searchablize [xs to-choose & to-search]
  (let []
    (->> xs
         (map (fn [m] {(str/join " " (flatten
                                      (if (sequential? to-search)
                                        (vals (select-keys m to-search))
                                        (vals m))))
                       (if (sequential? to-choose)
                         (select-keys m to-choose)
                         (to-choose m))}))
         ;;assure the to-search fields formed string is unique
         (apply merge)
         )
    ;
    ))

;;reverse index of hashmaps
(defn reverse-index [xs to-search & to-choose]
  (->> xs
       (mapcat (fn [m]
                 (let [x-or-xs (get m to-search)
                       v (if (empty? to-choose)
                           m
                           (if (= 1 (count to-choose))
                             ;;only one field, return the value
                             (get m (first to-choose))
                             ;;multiple fields, return selected hashmap
                             (select-keys m to-choose)))]

                   (if (sequential? x-or-xs)
                     (map (fn [k] {k v}) x-or-xs)
                     [{x-or-xs v}]))))
       ;;assure the to-search fields formed string is unique
       (apply merge)))


(defn unfold [m & ks]
  (->> ks
       (mapcat #(->> (% m)
                     (map(fn [x] (assoc m % x))))
  )))

;; f is the test function
;; edge is the value of test function expected
;;    i.e. [true false]
;;             means this edge is considerated as split point
;;                   first test function result is true
;;                   second test function result is true
;;
(defn group-by-edge
  ([xs f edge]
   (group-by-edge xs f edge [] []))
  ([xs f edge cur res]
   (let [[a b & more] xs]
     (println more)
     (if (nil? more)
       (conj res (conj cur b))
       (if (= [(f a) (f b)] edge)
         ;;edge matches new cur. conj previous to res
         (group-by-edge (cons b more) f edge
                        []
                        (conj res (conj cur a)))
         ;; not match, march one element
         (group-by-edge (cons b more) f edge
                        (conj cur a)
                        res)
         ;;
         )))))

;;like group-by but will merge the vals to be a hashmap
;;  use name-a-key to avoid collision
(defn merge-by-group-by [f coll]
  (vals (map-on-val #(apply merge %) (group-by f coll))))


(defn hashmap-by [f xs]
  (let [m (->> xs (group-by f))]
    (->> (get m false)
         (map (fn [x]
                {x (get m true)})))))

(defn ->words [s]
  (->> (str/split s #" |\n")
       (remove empty?)))


(defmacro gen-assoc [f]
  (let [fname (symbol (str "assoc-" (force-str f)))
        val (keyword (force-str f))]
    `(defn ~fname [h]
       (assoc h ~val (~f h)))))

(defn current-branch [path]
  (str/trim-newline
   (:out (run-cmd "cd " path "&&" "git branch --show-current"))
   )
  )

(defn git-push [path]
  (let [s (current-branch)]
    (run-cmd "cd " path "&&" "git push origin" (str s ":" s)))
  )

(defn git-pull [path]
  (let [s (current-branch)]
    (run-cmd "cd " path "&&" "git pull origin" (str s ":" s))))

(defn measure-run [f]
  (force-float
   (first (re-find #"(\d+\.\d+)"
                   (str (with-out-str (time (f))))))))

(defn time-ns [& xs ]
  (->> xs
   ;(all-ns)
   (map (comp symbol force-str))
   (map #(do (try (remove-ns %) (catch Exception e nil)) %))
   (map #(identity {:t (measure-run (partial require %)) :ns %}))
   (sort-by :t)
   )
  )

(defn sort-by-val [m]
  (sort-by (fn [[k v]] v) m))

(defn sorted-by-val [m]
  (into (sorted-map-by (fn [a b] (compare
                                   [(get m b) b]
                                   [(get m a) a]))) m)
  )

(defn reverse-sort-by-val [xs]
  (reverse (sort-by-val xs)))

(defn stat-count [xs k]
  (map (comp println (partial str/join " "))
       (reverse-sort-by-val
         (map-on-val count (group-by k xs))
   )
  ))

;; like count distinct in mysql
;; TODO support ks
(defn ->stat [xs k]
  (map-on-val count (group-by k xs))
  )

(defn quote-quote [s]
  (str/replace s "\"" "\\\"")
  )

(defn quote-str [s]
  (str "\"" s "\"")
  )

(defn val->key [[k xs]]
  (->> xs
       (map (fn [x] {x k}))))

;; seems not used.
;;(defn ->hashmap [m key-key val-key]
;;  (->> (seq m)
;;       (map (fn [[k v]] {key-key k val-key v}))
;;       ))

(defn lines-between [s from-s to-s]
  (let [from (str/index-of s from-s)
        to (str/index-of s to-s)]
    (if (empty? s)
      nil
      (-> (str/split-lines
           (subs s from to))
          rest
          drop-last))))

(defn val->str [m]
  (map-on-val (fn [x]
                (if (sequential? x)
                  (str/join "," x)
                  x))
              m
              )
  )

(defn str-or-file->ips [& xs]
  (let [s (str/join " " xs)
        ips (->ips s)]
    (if (empty? xs)
      ;;try read from in pipe
      ;;need to fix
      ;(->ips (slurp *in*))
      nil
      (if (empty? ips)
        (->> xs
             (map #(if (fs/exists? %)
                     (slurp %)
                     ""))
             (str/join " ")
             ->ips)
        ips))))

(defn str-or-file->lines [& xs]
  (let []
    (->> xs
         (mapcat #(if (fs/exists? %)
                    (slurp-lines %)
                    [%]
                    ))
         )
    )
  )

(defn quote-parenthese [s]
  (-> s
      (str/replace "(" "\\(")
      (str/replace ")" "\\)")))
(defn ->abs-path [x]
  (readlink "-f" x))

(defn alias [s x]
  (let [line (-> (str "alias " (str (force-str s) "=" "'" x "'"))
                 (quote-parenthese)
                 )
        f (if (osx?)
            (ls "$HOME/.bash_profile")
            (ls "$HOME/.bashrc"))]
    (when-not (in? (slurp-lines f) line)
      (spit-line f line))))

;;for seq of hashmaps
;;  do distinct by key 
;;  then choose same key element by choose-fn
(defn distinct-by
  ([xs k choose-fn]
   (vals (map-on-val choose-fn
                     (group-by k xs))))
  ([xs k]
   (distinct-by xs k first)))

(comment
  (->abs-path "tmp/1.i")
  (alias :vj "c https://zh.m.wikisource.org/wiki/%E9%87%91%E5%89%9B%E8%88%AC%E8%8B%A5%E6%B3%A2%E7%BE%85%E8%9C%9C%E7%B6%93_(%E9%B3%A9%E6%91%A9%E7%BE%85%E4%BB%80)")

  (str-or-file->ips "127.0.0.1")
  (str-or-file->ips "./1.i" "./2.i")

  (println
   (quote-quote "ab\""))

  ;;naive version. TODO: should unfold by itself.
  (unfold {:uni :☯
           :bin [:yin :yang]
           :oct [:☰ :☱ :☲ :☳ :☴ :☵ :☶ :☷]} :oct)

;;TODO automatic unfold
  (unfold {:meta {:uni :☯
                  :bin [:yin :yang]
                  :oct [:☰ :☱ :☲ :☳ :☴ :☵ :☶ :☷]}} [:meta :oct])

  (-> "abcAB.*?!"
      (range-replace-string "_" \A \Z)
      (range-replace-string "_" \a \z)
      (range-replace-string "_" \0 \9))

  (curl-cert-expire-date "www.163.com" "111.177.39.150" 443)

  (mock slow-repeater "abc" 3)

  (mock slow-repeater "abc" 6)

  (mock-clean slow-repeater "abc" 3)

  (->k=v {:PORT 123 :PROTO "tcp"} :prefix "--env")
  

  (alias :main "git checkout main")
  (alias :master "git checkout master")

  (def c
    (tail-f "/tmp/reboot.edn"))


  (async-fn #(println "trying: login" %) c )
  ;;
  )



(defn trim-leading [a b s]
  ;;a string 0 or * to be trimmed
  ;;b string only one to be trimmed
  (let [p0 (re-pattern (str b "(.*)"))
        p1 (re-pattern (str "(" a "*" ")"
                            b "(.*)"))
        xs (re-find p1 s)]
    (when-let [[_ trimmed res] (if (nil? xs)
                                 (re-find p0 s)
                                 xs)]
      {:count (count trimmed)
       :res res})))

(def trim-md (partial trim-leading "\\s" "- "))
(defn parse-md [lines]
  (let [xs (remove nil? (map trim-md (reverse lines)))]
    (loop [xs xs
           prev {:count nil :res ""}
           block '()
           result '()]
      (if (empty? xs)
        (cons block result)
        (let [{:keys [count res] :as cur} (first xs)]
          (println res block result "..." count (:count prev))
          (if (nil? res)
            (cons block result)
            (cond
              (nil? (:count prev)) (recur (rest xs) cur
                                          (cons res block)
                                          result)
              (= count (:count prev)) (recur (rest xs) cur
                                             (cons res block)
                                             result)
              (> count (:count prev)) (recur (rest xs) cur
                                             (list res)
                                             (cons block result))
              :else (recur (rest xs) cur
                           (cons res (list block))
                           result))

;;
            ))))))

(defn parse-md-file [f]
  (parse-md (slurp-lines f)))



