(ns knock.hanzi
  (:require [knock.utils :refer :all]
            [knock.browser :refer [go click click-multi driver]]
            [etaoin.api :as e]))

(defn cha [zi]
  (let [_ (go (format "https://hanyu.baidu.com/s?wd=%s&ptype=zici" zi))]
    {:strokes (e/get-element-attr driver "//*[@id='word_bishun']" :src)
     :meaning (e/get-element-inner-html driver "//*[@id='detailmean-wrapper']")}))



(comment
  (mock cha "慧")
  (mock cha "定")

  (go "https://hanyu.baidu.com/s?wd=慧&ptype=zici")
  (e/get-source driver)
  )



