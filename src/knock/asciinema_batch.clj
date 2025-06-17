(ns knock.asciinema-batch
  (:require [clojure.string :as str]
            [babashka.fs :as fs]
            [babashka.process :refer [shell]]))

(defn find-cast-files [dir]
  (let [result (shell {:out :string} "find" dir "-name" "*.cast" "-type" "f")
        output (str/trim (:out result))]
    (if (empty? output)
      []
      (str/split output #"\n"))))

(defn select-file-with-gum [files]
  (let [result (shell {:out :string :in (str/join "\n" (conj files "[Exit]"))} "gum" "filter")
        selected (str/trim (:out result))]
    selected))

(defn -main [& args]
  (let [dir (or (first args) ".")]
    (println "Looking for .cast files in:" dir)
    (let [cast-files (find-cast-files dir)]
      (if (empty? cast-files)
        (println "No .cast files found in" dir)
        (do
          (println "Found" (count cast-files) ".cast files:")
          (doseq [file cast-files]
            (println "-" (fs/file-name file)))
          
          ;; Loop for continuous file selection
          (loop []
            (let [selected-file (select-file-with-gum cast-files)]
              (cond
                (empty? selected-file)
                (println "No file selected. Exiting.")
                
                (= selected-file "[Exit]")
                (println "Exiting asciinema-batch.")
                
                :else
                (do
                  (println "Selected file:" selected-file)
                  (shell "bb" "asciinema-player" selected-file)
                  (println "\nReturning to file selection...")
                  (recur))))))))))
