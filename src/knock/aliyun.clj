(ns knock.aliyun
  (:require [knock.utils :refer :all]
            [clojure.string :as str]))

(defn change-password [username]
  (let [password (str/trim (run-cmd! :gen-password))]
    (println "changing password")
    ;;using aws cli: https://aws.amazon.com/cn/cli/ 
    ;;aws configure after installation
    (println (run-cmd :aliyun :ram :UpdateLoginProfile "--UserName" username "--Password" (str "'" password "'")))
    (pbcopy password)
    (println "check the password in clipboard, or just CMD + V")))



