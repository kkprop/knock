(ns knock.book
  (:require [knock.utils :refer :all]
            [babashka.fs :as fs]
            [clojure.string :as string]
            [babashka.pods :as pods]
            [knock.tui :as tui]
            [bblgum.core :as b]
            [clojure.string :as str]
            [knock.utils :as utils]))

(defn pdf [path]
  (:out
   (run-cmd "pdftohtml" "-hidden"  "-stdout" (str "'" path "'"))))



(defn epub [path]
  (:out
   (run-cmd "einfo" "-p" (str "'" path "'"))))

(defn tmp-file [s]
  (let [f (str "/tmp/" (uuid) ".tmp")]
    (spit f s)
    f)
  )

;;anything to html
;; url
;; books: pdf epub 
(defn ->html [path]
  (if (str/ends-with? path ".epub")
    (mock epub path)
    (if (str/ends-with? path ".pdf")
      (mock pdf path)
      (if (str/starts-with? path "http")
        (:body (utils/curl-get path))
        (mock slurp path)
        ))))
;;currently is mock
;;a path is a uri
;; once this uri is cached, always return the same content
(defn markdown [path]
  (:out
   (run-cmd "pandoc" "-f html" "-t plain "
            (tmp-file (mock ->html path)
            ;;
                      ))))

(defn pick [path]
  (let [f (tmp-file (markdown path))]
    (tui/filter f)
    ))


(utils/config :book-dir)

(defn fuzzy-open [s & {:keys [dir]
                       :or {dir (if-not (nil? book-dir)
                                  book-dir
                                  ".")}}]

  (let [xs (fs/list-dir dir)]
    (pick
     (tui/filter (tmp-file (str/join "\n" xs)))))
  ;;
  )

(comment
  (fuzzy-open "木")

  (tmp-file
   (.getByte
    (mock ->html "https://www.cfolu.com/xiuxueyd/gongan/0001.html")
    ""))

  (tmp-file
   (mock ->html "https://ctext.org/zhuangzi/enjoyment-in-untroubled-ease/zhs")
   )

  (def path "resources/babooka.pdf")

  (pdf "resources/babooka.pdf")
  (markdown "resources/babooka.pdf")
  (pick "resources/babooka.pdf")

  (mock epub "resources/babooka.epub")

  (->html  "resources/babooka.epub")
  (->html  "resources/babooka.pdf")
  ;;TODO support picture 
  (->html "/Users/dc/Downloads/books/Where The Wild Things Are (Maurice Sendak) (Z-Library).pdf")
  (->html "/Users/dc/Downloads/books/The Ethics, Parts 1-5 by Benedict de Spinoza (Translated by R. H. M. Elwes) (z-lib.org).pdf")

;;k
  )
