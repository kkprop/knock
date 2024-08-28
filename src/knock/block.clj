(ns knock.block
  (:require [knock.utils :refer :all]
            [clojure.string :as str]
            [babashka.fs :as fs]
            [babashka.process :as p]
            ))

;;block is the fundamental component of tree shape data
:block/string

:block/refs

:block/children

;;drive the process of using blokcs by inspecting this file itself.


(defn block [s]
  {:block/string s}
  )


;; a charactor for chinese
;; a word for other language
(defn ->units [x]
  (let [s (str-or-file x)]
    (->>
     (str/split-lines s)
     (mapcat seq)
     )
    ;;
    ))

(defn nx [n f xs]
  ((apply comp (repeat n f))
   xs
   ))

(defn ->phrase
  ([i n xs]
   (let  [m (map-on-val count (group-by identity
                                        (partition-all n (nx i rest xs))))]

     m))
  ([n xs]
   ;;first
   (let [m (->> (range n)
                (map #(->phrase % n xs))
                (apply merge-with +))
         mm (map-on-key #(apply str %) m)]
     (into (sorted-map-by (fn [a b] (compare
                                     [(get mm b) b]
                                     [(get mm a) a]))) mm)))

  ([xs]
   (if (string? xs)
     (->phrase 1 (->units xs))
     (->phrase 1 xs))))


(defn mosaic [s xs]
  (let [cur (atom [])]
    (doseq [[i x] (map-indexed list s)]
      (if (in? xs i)
        (swap! cur conj " ")
        (swap! cur conj x)))
    (apply str @cur)))

(comment
  (mosaic "abc" [1 2])
  (->phrase s)
  s
  )


(defn tripler
  ([s]
   ;;(if (sequential? s)
   ;;  [apply tripler s]
     (tripler s (range (count s)) 0)
   ;;)
   )
  ([s xs n]
   (let [xs (apply list (drop (* 3 (+ 1 n)) (shuffle xs)))]
     [(mosaic s xs) xs (+ 1 n)]
     ;;
     )))


(defn lazy-read
  ([s]
   (do
     (tmux s)
     (let [id (->uuid s)
           cur (atom (tripler s))
           p (promise)
           ]
       (thread!
        (while (not (realized? p))
          (clear id)
          (send-text id (first @cur))
          (Thread/sleep 1100))
          )
       (->> (range 3)
            (map (fn [i]
                   (let [xs (apply tripler @cur)]
                     (reset! cur xs)
                     )
                   (Thread/sleep 1100)
                   ))
            (apply list)
            )
       (deliver p :done)
       )
     )

   ;;@(promise)
   ))


(comment


  (sequential? (tripler s))
  (repeatedly (apply tripler (tripler s)
                     )
              3)
  (->uuid s)

  (pp
   (def s "大學之道，在明明德，在親民，在止於至善。知止而後有定，定而後能靜，靜而後能安，安而後能慮，慮而後能得。物有本末，事有終始，知所先後，則近道矣。古之欲明明德於天下者，先治其國；欲治其國者，先齊其家；欲齊其家者，先脩其身；欲脩其身者，先正其心；欲正其心者，先誠其意；欲誠其意者，先致其知，致知在格物。物格而後知至，知至而後意誠，意誠而後心正，心正而後身脩，身脩而後家齊，家齊而後國治，國治而後天下平。自天子以至於庶人，壹是皆以脩身爲本。其本亂而末治者否矣，其所厚者薄，而其所薄者厚，未之有也！此謂知本，此謂知之至也。"))

  (pp
   (sorted-by-val
    (->phrase 1 3
              (->units "说你又不听，听你又不懂，懂你又不做，做你又做错，错你又不认，认你又不服，不服你又不出声"
                ;(->abs-path "~/shurangama")
                       ))))
  (pp
   (->phrase 2 (->units "蜕变： 不喝酒了改喝茶了， 不抽烟了改熏香了， 不听歌了改听戏了， 专心烧香拜佛和赚钱")))

  (->units "The Limits of my language are the limits of my world")

  (run-cmd :clear)
  ;;
  )



(defn verticalize [xs]
  ;;full size
  (let [c (apply max (map count xs))
        n 16
        ln (count xs)]
    (->> xs
         (map #(fill-after % n \u3000))
         (reverse)
         (apply interleave)
         (partition-all ln)
         (map (partial apply list))
         (map (partial str/join ""))
         (map #(fill-before % n \u3000))
         ;;fill to n(16), to make it seems starting with right 
         ;(map #(str/replace % "。。" (str "。" \u3000)))
         (str/join "\n")))

;;
  )
;;to vertical
(defn ->shu [s]
  (let [s (if (fs/exists? s) (slurp s) s)
        n 16
        ;have to use \u3000 to file. which cause align problem when it is convert to single space
        ;。。connected caused problem

        xs (map (partial str/join) (mapcat #(partition-all n %) (str/split-lines s)))
        ;;to square
        ;xs (->> (partition-all 16 (str/join "" (str/split-lines s))) (map (partial str/join)))
        ]

    (->>
     (partition-all n xs)
     (map verticalize)
     (str/join (apply str "\n" (apply str (repeat (* 2 n) "-")) "\n"))
     println))

;;
  )



(comment

  (def s
    "已去無有去
未去亦無去
離已去未去
去時亦無去")
  (def xs  (str/split-lines s))
  ;;
  )

(def lp "lift-pause")
(def mp (atom (mouse-pos)))

(defn need-pause? []
  (and
   (not= (mouse-pos) @mp)
   ;;not lifting pause
   (not (on? lp))
   ))

(defn paste-roam []
  (let [s (pbpaste)]
    (when-not (= s bub)
      (let [s (if (str/ends-with? s bub) (chop-to! s bub)
                  s)]
        ;(send-keys* "Roam Research" [:v :command :shift])
          (send-keys* "Roam Research" :ctrl)
          (send-text* "Roam Research" s)
          (send-keys* "Roam Research" :enter))
        )))

(defn play-bubble
  ([] (play-bubble 1000))
  ([interval]
   (let [interval (if (nil? interval) 200 interval)]
     (let [s (bubble-trim (pbpaste))
           x (if (str/includes? s "。") "。" ".")
           _ (pbcopy s)]
       (make-bubble)
       (cbubble x)
       (paste-roam)
       ;;still have 
       (while (not (bubble?))
         (cbubble x)
         (paste-roam)
         (pause 200)
         (while (need-pause?)
           (pause 200))
         ;;lift pause already works once. remove
         (when (on? lp)
           (off! lp)
           ;;also update mouse pos
           (reset! mp (mouse-pos))
           )
         ;;
         )))))



