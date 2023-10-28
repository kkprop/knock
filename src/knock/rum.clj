(ns knock.rum
  (:require [knock.utils :refer :all]
            ;[rewrite-clj.zip :refer  ]
            )
  )

;;first to ruminate is bb.edn

(defn time-requires
  ([] (time-requires "./bb.edn"))
  ([path]
   (let [bb-edn (load-edn path)
         {{:keys [requires]
           :as tasks} :tasks} bb-edn]

     (->> requires (map #(assoc bb-edn :tasks
                                (assoc tasks :requires (symbol (str "'(" % ")") )
                                       (symbol "pp-ns")
                                       (symbol (str "(ns-publics '" (str (first %)) ")")))))
          (map #(do (pp-spit (str
                               (today) 
                               ".edn") %)
                    )
               )
          ))
;;tasks
     ))



(comment

  (keys
   (bb-edn))

  (time-requires)

  )
