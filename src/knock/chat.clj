(ns knock.chat
  (:require [knock.utils :refer :all]
            [babashka.curl :as curl]
            )
  )
(env :OPENAI_API_KEY)



(defn prompt [q]
  (let [res (curl-any curl/post
                      ;;doc :https://platform.openai.com/docs/api-reference/completions/object
                      "https://api.openai.com/v1/chat/completions"
                      :headers {"Content-Type" "application/json"
                                "Authorization" (str "Bearer " OPENAI_API_KEY)}
                      :body {:messages [{:role "user"
                                         :content q}]
                             :model "gpt-3.5-turbo-0613"})]
    (:choices/message/content
     (slash-flatten-map (:body res)))))


(comment
  ;;
    (mock prompt "what is socrate question method")
;
)
