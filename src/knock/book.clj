(ns knock.book
  (:require [knock.utils :refer :all]
            [babashka.fs :as fs]
            [clojure.string :as string]
            [clojure.string :as str])
  )


(defn pdf [path]
  (:out
   (run-cmd "pdftohtml" "-stdout" (str "'" path "'"))))


(defn epub [path]
  (:out
   (run-cmd "einfo" "-p" (str "'" path "'"))))


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
  ;; support picture 
  (->html "/Users/dc/Downloads/books/Where The Wild Things Are (Maurice Sendak) (Z-Library).pdf")
  (->html "/Users/dc/Downloads/books/The Ethics, Parts 1-5 by Benedict de Spinoza (Translated by R. H. M. Elwes) (z-lib.org).pdf")

;;k
  )