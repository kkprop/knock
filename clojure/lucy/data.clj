(ns lucy.data
  (:require [datascript.core :as d :refer [squuid]]
            [datascript.db  :as db]
            [clojure.string :as str]
            [babashka.fs :as fs]
            [datascript.transit :as dt :refer [read-transit-str write-transit-str]]
            [clojure.java.io :as io]
            [knock.utils :as utils :refer [map-on-val fuzzy-matched?
                                           slash-flatten-map]]
            [clojure.string :as string])
  (:import [datascript.db DB Datom]
           [java.io ByteArrayInputStream ByteArrayOutputStream]
           [java.util UUID]))

(declare load-db)
(defonce ^:dynamic dbs (atom {}))


(defn db [path]
  (let [m (get @dbs path)]
    (if (nil? m)
      (let [cur (load-db path)]
        (swap! dbs merge {path cur})
        cur)
      m
      )))

(defn load-roam-edn [path]
  (db/db-from-reader
   (utils/load-edn path :readers {'datascript/DB identity})))

(defn dump-conn [c]
    (let [tx (d/transact! c [{:db/id -1
                              :cur-time 0}])]
      (dt/write-transit-str (:db-after tx))))

(defn dump-db [db path]
  (spit path (dt/write-transit-str db)))

;;set block/uid to be the unique identity of an entity
(defn empty-conn [] (d/create-conn {:block/uid {:db/unique :db.unique/identity}}))

(defn load-roam-edn [path]
  (db/db-from-reader
   (utils/load-edn path :readers {'datascript/DB identity})
   )
  )

(defn load-db [path]
  (let [_ (if-not (fs/exists? path)
            (do
              ;;todo create dir as well?
              (spit path (dump-conn (empty-conn)))))
        db (if (str/ends-with? path ".edn")
              (load-roam-edn path)
             (dt/read-transit-str (slurp path)))]

    (d/init-db (:eavt db) (:schema db))))

;;wrap history function calling facts into datascript
(defn tracking [db-path f & args]
  (let [i {:db/id -1
           :req args
           :fn (str (type f))
           :res (apply f args)}
        _ (clojure.java.io/make-parents db-path)
        db (load-db db-path)]
    (dump-db (d/db-with db [i]) db-path)
    (:res i)))

;; floating things below.

(defn db-pull-all [db]
  (mapcat identity
          (d/q '[:find (pull ?e [*])
                 :in $
                 :where [?e]]
               db)))

(defn block-add-expr [m {:keys [key user]
                         :or {key nil
                              user "matrix"}
                         :as opts}]
  (let [m (slash-flatten-map m)
        m (assoc m :db/user user)
        uid (if (nil? key)
              (squuid)
              (key m)
              )
        fields (map #(into [] (concat [:db/add -1]
                                      %))
                    (map-on-val #(if (nil? %)
                                   "nil"
                                   %) m))
               ;;
        ]
    (vec (cons [:db/add -1 :block/uid uid]
               fields))
;;
    ))

(defn write-db [db-name after]
  (swap! dbs merge {db-name after}))

(defn write-db! [db-name]
  (dump-db (db db-name) db-name)
  )

(defn insert [db-name m {:keys [key user]
                            :as opts}]
  (let [cur (db db-name)
        after (d/db-with cur (block-add-expr m opts))
        ;;
        ]
    (write-db db-name after)
    ))

(defn insert! [db-name m {:keys [key user]
                          :as opts}]
  (insert db-name m opts)
  (write-db! db-name)
  )

;;related query functions
;;pull block by eid
(defn entity [db eid]
  (let [db (if (string? db) (db db) db)]
    (d/pull db '[*] eid)
  ))


;;filter block by attr value
(defn attr-value [db attr value]
  (let [db (if (string? db) (db db) db)]
    (flatten
     (d/q '[:find (pull ?e [*])
            :in $ ?a ?v
            :where [?e ?a ?v]] db attr value))))

(defn has-attr [db attr]
  (let [db (if (string? db) (db db) db)]
    (flatten
     (d/q '[:find (pull ?e [*])
            :in $ ?a
            :where [?e ?a _]] db attr ))))

;;filter block by value in :attr.
;; i.e. a block have :block/children ["LrGj93uYu" "lMAj01xvc"]
;;  call backward-attr-value :block/chidren "LrGj93uYu" will return the block
(defn backward-attr-value [db attr value]
  (let [db (if (string? db) (db db) db)]
    (flatten
     (d/q '[:find (pull ?e [*])
            :in $ ?a ?v
            :where [?e ?a ?refs]
            [?refs ?a]
            ] db attr value))))
(defn fuzzy-attr-substr [db attr substr]
  (let [db (if (string? db) (db db) db)]
    (flatten
      (d/q '[:find (pull ?e [*])
             :in $ ?a ?ss
             :where [?e ?a ?str]
             [(clojure.string/includes? ?str ?ss)]] db attr substr))))

;;block by :block/uid
(defn block [db uid]
  (first
   (attr-value db :block/uid uid)))

;;string fuzzy much
(defn attr-substr [db attr substr ]
  (let [db (if (string? db) (db db) db)]
    (flatten
     (d/q '[:find (pull ?e [*])
            :in $ ?a ?ss
            :where [?e ?a ?str]
            [(clojure.string/includes? ?str ?ss)]] db attr substr))))
;;

(defn pull-nested-block [db eid attr]
  (let [db (if (string? db) (db db) db)
        b (entity db eid)
        ;;hash-map or a list of hash-map
        v (get b attr)]
    (if (map? v)
      (if (nil? (:db/id v))
        v
        (entity db (:db/id v)))
      (if (sequential? v)
        (->> v
             (map :db/id)
             ;;debug
             ;;(map #(entity db %))
             vec
             )
        v
        )
      )
    ))

(defn child-blocks [db eid]
  (pull-nested-block db eid :block/children))

(defn parent-blocks [db eid]
  (pull-nested-block db eid :block/parents))


;;expand netsted db/id objects only one level
(defn block-expand-l1 [db eid]
  (let [m (entity db eid)]
    (->> (keys m)
         (map #(assoc {} % (pull-nested-block db (:db/id m) %)))
         ;;(merge)
         )
    ))



(defn rand-block
  ([db]
   (entity db
           (+ 1
              (rand-int (:max-eid db)))))
  ([db title]
   (let [uid (:block/uid (attr-value db :node/title title))]
     (backward-attr-value db :block/refs uid))))

(defn ids [db]
  (range 1 (+ 1 (:max-eid db)))
  )

(defn rand-n-eid [db n]
  (take n (shuffle (ids db)))
  )

(defn rand-n-entity [db n]
  (->> (rand-n-eid db n)
       (map (partial entity db))
       (vec)
       )
  )


;;return eid -> :block/backrefs
(defn backrefs [db]
  (->>
   (range 1 (+ 1 (:max-eid db)))
   (map #(entity db %))
   (mapcat #(zipmap
             (repeat (:db/id %))))
   (reduce (fn [a [eid id]]
             (assoc a eid
                    (conj (get a eid []) id))) {})))

;; todo apply to cache
;; problem no way to do the ! for a db
(defn backref! [db eid & id]
  (let [m (entity db eid)]
    (assoc m :block/backrefs
           (vec (concat (get m :block/refs []) id)))
    ;;
    ))

(defn update-by-eid [db-name eid {:keys []
                       :as opts}]
  (let [cur (db db-name)
        m (dissoc (entity cur eid) :db/id)
        new (merge m opts)
        expr (->> new (map #(concat [:db/add eid] %)) (map vec))
        after (d/db-with cur expr)
        ]
    (write-db db-name after)
    ))

(defn update! [db-name eid {:keys []
                       :as opts}]
  (update-by-eid db-name eid opts)
  (write-db! db-name)
  )


(defn range-eid [db]
  (range 1 (+ 1 (:max-eid db)))
  )

(defn range-entity [db]
  (let [db (if (string? db) (db db) db)]
    (->> (range-eid db)
         (map (partial entity db)))))


(comment
  (rand-n-entity (db "song.db") 3)

  ;;this file is really slow
  (def dc (db "1.edn"))
  (utils/config tpc)
  (has-attr (db tpc) :node/title)
  (def dc (db "1.edn"))
  (def uid "LrGj93uYu")
  (def eid
    (:db/id (block dc uid)))
  ;;(def uid "yh9a0tmva")

  (map (juxt :block/uid :block/page :block/parents)
       (attr-value dc :block/string "__communication__"))

  (entity dc 72)
  (entity dc 5)
  (entity dc 194)

  (attr-value dc :node/title "Artificial Intelligence")
  (attr-value dc :node/title "Artificial Intelligence")

  (count
   (map (juxt :node/title :block/uid :db/id :block/page)
        (attr-substr dc :node/title "meta")))

  (child-blocks dc "LrGj93uYu")
  (parent-blocks dc "LrGj93uYu")
  (rand-block dc)
  (rand-block dc "autorecording")

  (attr-value dc :node/title "autorecording")

  (block-expand-l1 dc eid)
  (block dc uid)
  (entity dc 1)
  (pull-nested-block dc eid :block/children)

  (backrefs dc)
  (:max-eid dc))

  

;;
