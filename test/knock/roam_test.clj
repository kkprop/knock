(ns knock.roam-test
  (:require [knock.roam :as sut :refer :all]
            [knock.utils :as utils :refer :all]
            [clojure.test :as t :refer :all]
            )
  )


(deftest test-pull-daily-notes []
  (def gt (load-edn "/Users/dc/dc.edn"))
  ;;(def gt g)

  (pull gt '[:find ?block-str
          :in $ ?uid
          :where [?b :block/uid ?block-uid]
          ]
     )

  (q gt '[:find ?e ?r
          :in $ ?uid
          :where [?e :block/uid ?uid]
          [?e :block/refs ?r]
          ;[?t :node/title ?e]
          ;[?n :node/title ?e]
          ] "OHPq8SeUu" )

  (search-block gt "40年来")

  ;; not working due to recent daily pages have no refs anymore
  ;; seems that the  cache is missing.
  (pull gt '[:block/uid (cur-daily-page)]
        '[:block/uid :node/title :block/string
          {:block/children [:block/uid :block/string]}
          {:block/refs [:node/title :block/string :block/uid]}]
                    )
;
  )






