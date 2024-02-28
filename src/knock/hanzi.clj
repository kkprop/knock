(ns knock.hanzi
  (:require [knock.utils :refer :all]
            [knock.browser :refer [go driver]]
            [knock.server :as server]
            [etaoin.api :as e]
            ))

(defn cha [zi]
  (let []
    (e/with-chrome-headless driver
      (e/go driver (format "https://hanyu.baidu.com/s?wd=%s&ptype=zici" zi))
      {:strokes (e/get-element-attr driver "//*[@id='word_bishun']" :src)
       :meaning (e/get-element-inner-html driver "//*[@id='detailmean-wrapper']")})))

(defn gif-show [url]
  (run-cmd :open url)
  )

(defn show [{:keys[strokes]
             :as opts}]
  (gif-show strokes)
  opts
  )

(defn stroke [zi]
  (:strokes (mock cha zi))
  )


(defn -main []
  (let []
    (server/stop-server)
    (server/run-ns {:ip "127.0.0.1" :port 1219} 'knock.hanzi)
    (@(promise))))


(comment
  (show
   (cha "慧")
   )

  (var-meta stroke)

  (cha "定")

  (stroke "定")
  (mock cha "律")
  (go "https://hanyu.baidu.com/s?wd=慧&ptype=zici")

    ;;
  )




