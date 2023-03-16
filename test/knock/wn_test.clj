(ns knock-test
  (:require
   [knock.wn :as wn :refer :all]
   [clojure.test :as t :refer :all]))



(deftest test-make-url []
  (is (= (search-wn "Hermann Hesse") 
         (search-wn "Hermann" "Hesse"))))



(run-tests)
