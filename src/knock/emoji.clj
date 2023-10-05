(ns knock.emoji
  (:require [emoji.core :as e]
            [knock.tui :as ui]
            [knock.utils :as utils :refer :all]
            [clojure.string :as str]))


(def emojis e/emojis)

(defn rand-n-emoji [n]
  (repeatedly n (partial rand-nth emojis)))

(defn i-emoji []
  (ui/search emojis :emojiChar
             :emoji :tags :description :aliases))

(comment
  (searchablize emojis :emojiChar
                ;;choose mulitple fields to search
                :emoji :tags :description :aliases)
  ;;search only one field
  (searchablize emojis :emojiChar)
  ;;search multiple field
  (searchablize emojis [:emojiChar :tags])

  (e/->alias "👍")
  (e/->alias "[666]")
  (e/->alias "🤔")

  (count emojis)
  (->>
   (rand-n-emoji 3)
   (map (juxt :emoji :description)))
  ;;
  )
