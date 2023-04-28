(ns knock.jp
  (:require [knock.browser :refer [go click click-multi driver]]
            [etaoin.api :as e]))


(def voice {:class "concept_audio concept_light-status_link"})
(def text {:class "concept_light-representation"})

(defn search-word [word]
  (go (str "https://jisho.org/search/" word))
  (try 
     (click voice)
     (catch Exception e "no voice to click")
     )
  (try 
    (e/get-element-text driver text)
    (catch Exception e nil)
    )
  ;;
  )



(comment
  (search-word "経度")
  (search-word "意")
  (search-word "心")
  (search-word "慈")

  ;;need to convert
  (search-word "识")

  (search-word "ここ")
;;
  )
