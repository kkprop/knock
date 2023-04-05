(ns knock.jp
  (:require [knock.browser :refer [go click click-multi driver]]
            [etaoin.api :as e]))


(def voice {:class "concept_audio concept_light-status_link"})
(def text {:class "concept_light-representation"})

(defn search-word [word]
  (go (str "https://jisho.org/search/" word))
  (when-not (nil? (e/get-element-inner-html driver voice))
    (click voice))
  (e/get-element-text driver text)
  ;;
  )



(comment
  (search-word "心")
  (search-word "意")
  (search-word "识")
  (search-word "慈")
;;
  )
