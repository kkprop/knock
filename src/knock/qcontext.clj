(ns knock.qcontext
  (:require [babashka.fs :as fs]
            [babashka.process :as p :refer [shell]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn expand-home
  "Expands the tilde (~) in a path to the user's home directory."
  [path]
  (if (str/starts-with? path "~")
    (str (System/getProperty "user.home") (subs path 1))
    path))

(defn list-context-files
  "Lists all .context files in the specified directory."
  [dir]
  (let [expanded-dir (expand-home dir)]
    (when (fs/exists? expanded-dir)
      (->> (fs/list-dir expanded-dir)
           (filter #(str/ends-with? (str %) ".context"))
           (map #(fs/file-name %))
           (sort)))))

(defn gum-available?
  "Checks if the gum command is available."
  []
  (try
    (p/shell {:out :string :err :string} "which" "gum")
    true
    (catch Exception _
      false)))

(defn expect-available?
  "Checks if the expect command is available."
  []
  (try
    (p/shell {:out :string :err :string} "which" "expect")
    true
    (catch Exception _
      false)))

(defn is-macos?
  "Checks if the system is macOS."
  []
  (str/includes? (str/lower-case (System/getProperty "os.name")) "mac"))

(defn select-file-manually
  "Prompts the user to select a file from the list using manual input."
  [files]
  (println "Available context files:")
  (doseq [[idx file] (map-indexed vector files)]
    (println (format "[%d] %s" (inc idx) file)))
  (println "[0] Exit/Cancel")
  (print "Select a file (number): ")
  (flush)
  (let [input (read-line)
        idx (try (Integer/parseInt input)
                 (catch Exception _ 0))]
    (cond
      (= idx 0) nil
      (and (> idx 0) (<= idx (count files))) (nth files (dec idx))
      :else (do
              (println "Invalid selection. Please try again.")
              (select-file-manually files)))))

(defn select-file-with-gum
  "Uses gum filter to let the user select a context file."
  [files]
  (when (seq files)
    (println "Found" (count files) "context files.")
    
    (if (gum-available?)
      (do
        (println "Use gum filter to select a file (type to filter, arrow keys to navigate, Enter to select):")
        (try
          (let [result (p/shell {:out :string :in (str/join "\n" (conj files "[Exit]"))} "gum" "filter")
                selected (str/trim (:out result))]
            (when-not (= selected "[Exit]")
              selected))
          (catch Exception e
            (println "Error using gum filter:" (ex-message e))
            (println "Falling back to manual selection...")
            (select-file-manually files))))
      (do
        (println "Note: 'gum' command not found. Using manual selection.")
        (select-file-manually files)))))

(defn run-q-with-context
  "Runs Amazon Q CLI with the selected context file."
  [context-dir file]
  (let [expanded-dir (expand-home context-dir)
        ;; Ensure we don't have double slashes in the path
        full-path (str (fs/path expanded-dir file))]
    (println (format "Starting Amazon Q with context file: %s" file))
    (println (format "Full path: %s" full-path))
    (println "Type your queries after Amazon Q starts...")
    (println)
    
    ;; Check if expect is available
    (if (expect-available?)
      ;; Use expect to automate the initial command
      (let [temp-script (fs/create-temp-file {:prefix "q-context-" :suffix ".exp"})
            temp-script-path (str temp-script)
            script-content (format "#!/usr/bin/env expect\n\nspawn q chat\nexpect \">\"\nsend \"refer %s\\r\"\ninteract\n" full-path)]
        
        (println "Debug: Using expect script:" temp-script-path)
        
        ;; Write the script content
        (spit temp-script-path script-content)
        
        ;; Make the script executable
        (fs/set-posix-file-permissions temp-script-path "rwxr-xr--")
        
        ;; Run the script
        (try
          (-> (p/process ["expect" temp-script-path]
                        {:inherit true})
              (p/check))
          (finally
            ;; Clean up the temporary script
            (fs/delete temp-script))))
      
      ;; Fallback to a simpler approach if expect is not available
      (let [temp-script (fs/create-temp-file {:prefix "q-context-" :suffix ".sh"})
            temp-script-path (str temp-script)
            script-content (format "#!/bin/bash\n\n# Start q chat\necho \"Starting Amazon Q...\"\necho \"When the prompt appears, type: refer %s\"\nq chat\n" full-path)]
        
        (println "Debug: Using bash script (expect not available):" temp-script-path)
        
        ;; Write the script content
        (spit temp-script-path script-content)
        
        ;; Make the script executable
        (fs/set-posix-file-permissions temp-script-path "rwxr-xr--")
        
        ;; Run the script
        (try
          (-> (p/process ["bash" temp-script-path]
                        {:inherit true})
              (p/check))
          (finally
            ;; Clean up the temporary script
            (fs/delete temp-script)))))))

(defn q-context
  "Main function to run the q-context task.
   Allows selecting a context file and launching Amazon Q with it."
  [& args]
  ;; Check if q command is available
  (try
    (p/shell {:out :string :err :string} "which" "q")
    (catch Exception _
      (println "Error: Amazon Q CLI (q) is not installed or not in PATH.")
      (println "Please install Amazon Q CLI and try again.")
      (System/exit 1)))
  
  (let [context-dir (if (seq args) (first args) "~/q")]
    (println (format "Using context directory: %s" context-dir))
    (if-let [files (list-context-files context-dir)]
      (if-let [selected (select-file-with-gum files)]
        (run-q-with-context context-dir selected)
        (println "Selection cancelled or no file selected."))
      (println (format "No .context files found in %s" context-dir)))))

(defn -main [& args]
  (apply q-context args))
