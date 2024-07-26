(ns knock.aws
  (:require [knock.utils :refer :all]
            [clojure.string :as str])
  )



(defn change-password [username]
  (let [password (str/trim (run-cmd! :gen-password))]
    (println "changing password")
    ;;using aws cli: https://aws.amazon.com/cn/cli/ 
    ;;aws configure after installation
    (println (run-cmd :aws :iam :update-login-profile "--user-name" username "--password" (str "'" password "'")))
    (pbcopy password)
    (println "check the password in clipboard, or just CMD + V"))
  ;
  )

(comment
  (def username "chengdongxu")
  )
