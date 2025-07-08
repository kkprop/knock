(ns knock.qreplay
  (:require [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [knock.asciinema-player :as ap]))

(defn list-cast-files []
  "List all .cast files in the ~/rec/ directory"
  (let [rec-dir (io/file (str (System/getProperty "user.home") "/rec"))]
    (if (.exists rec-dir)
      (->> (.listFiles rec-dir)
           (filter #(.isFile %))
           (filter #(str/ends-with? (.getName %) ".cast"))
           (map #(.getName %))
           (sort))
      [])))

(defn select-cast-file [cast-files]
  "Use gum filter to select a cast file"
  (when (seq cast-files)
    (let [temp-file (str "/tmp/cast-files-" (System/currentTimeMillis) ".txt")]
      (try
        ;; Write the list to a temporary file
        (spit temp-file (str/join "\n" cast-files))
        ;; Use bash to properly handle the pipe
        (let [result (p/shell {:out :string :continue true}
                             "bash" "-c" (str "cat " temp-file " | gum filter --height 10"))]
          (when (zero? (:exit result))
            (let [selected (str/trim (:out result))]
              (when-not (str/blank? selected)
                selected))))
        (catch Exception e
          nil)
        (finally
          ;; Clean up temp file
          (when (.exists (io/file temp-file))
            (.delete (io/file temp-file))))))))

(defn run-asciinema-player [cast-file]
  "Run asciinema-player on the selected cast file"
  (let [full-path (str (System/getProperty "user.home") "/rec/" cast-file)]
    (ap/-main "--input" full-path)
    ;; Wait for user confirmation before returning to menu
    (println "\nPress Enter to return to main menu...")
    (read-line)))

(defn main-loop []
  "Main loop for selecting and playing cast files"
  (loop []
    (let [cast-files (list-cast-files)]
      (if (empty? cast-files)
        (println "No .cast files found in ~/rec/")
        (if-let [selected-file (select-cast-file cast-files)]
          (do
            (run-asciinema-player selected-file)
            (p/shell ["clear"])
            (recur))
          (println "No file selected - exiting"))))))

(defn -main [& args]
  (main-loop))
