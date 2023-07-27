(ns knock.book
  (:require [knock.utils :refer :all]
            [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.string :as str])
  )


(defn pdf [path]
  (:out
   (run-cmd "pdftohtml" "-stdout" path)))


(defn epub [path]
  (:out
   (run-cmd "einfo" "-p" path)))


(defn ->html [path]
  (if (str/ends-with? path ".epub")
    (epub path)
    (if (str/ends-with? path ".pdf")
      (pdf path)
      (if (str/ends-with? path ".html")
        (slurp path)
          nil
        )
      )))

(comment

  (pdf "resources/babooka.pdf")

  (epub "resources/babooka.epub")

  (->html  "resources/babooka.epub")
  (->html  "resources/babooka.pdf")
;;k
  )
