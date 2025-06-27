(ns knock.qcontext
  (:require [babashka.fs :as fs]
            [babashka.process :as p :refer [shell]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [knock.utils :refer [cur-time-str]]))

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
  (println "[1] nil.context - No context file")
  (doseq [[idx file] (map-indexed vector files)]
    (println (format "[%d] %s" (+ idx 2) file)))
  (println "[0] Exit/Cancel")
  (print "Select a file (number): ")
  (flush)
  (let [input (read-line)
        idx (try (Integer/parseInt input)
                 (catch Exception _ 0))]
    (cond
      (= idx 0) nil
      (= idx 1) "nil.context"
      (and (>= idx 2) (<= idx (inc (count files)))) (nth files (- idx 2))
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
          (let [result (p/shell {:out :string :in (str/join "\n" (concat ["[nil.context - No context file]"] files ["[Exit]"]))} "gum" "filter")
                selected (str/trim (:out result))]
            (cond
              (= selected "[Exit]") nil
              (= selected "[nil.context - No context file]") "nil.context"
              :else selected))
          (catch Exception e
            (println "Error using gum filter:" (ex-message e))
            (println "Falling back to manual selection...")
            (select-file-manually files))))
      (do
        (println "Note: 'gum' command not found. Using manual selection.")
        (select-file-manually files)))))

(defn asciinema-available?
  "Checks if the asciinema command is available."
  []
  (try
    (p/shell {:out :string :err :string} "which" "asciinema")
    true
    (catch Exception _
      false)))

(defn generate-recording-filename
  "Generates a filename for asciinema recording based on context file and current time."
  [context-file]
  (let [context-name (-> context-file
                         (str/replace #"\.context$" "")
                         (str/replace #"[^a-zA-Z0-9\-_]" "-"))
        timestamp (cur-time-str "yyyy-MM-dd-HH-mm-ss")]
    (str context-name "-" timestamp ".cast")))

(defn ensure-rec-directory
  "Ensures the ~/rec directory exists."
  []
  (let [rec-dir (str (System/getProperty "user.home") "/rec")]
    (when-not (fs/exists? rec-dir)
      (fs/create-dirs rec-dir))
    rec-dir))

(defn run-q-with-context
  "Runs Amazon Q CLI with the selected context file."
  [context-dir file]
  (let [expanded-dir (expand-home context-dir)
        ;; Ensure we don't have double slashes in the path
        full-path (when-not (= file "nil.context") 
                    (str (fs/path expanded-dir file)))
        ;; Setup recording
        rec-dir (ensure-rec-directory)
        recording-filename (generate-recording-filename (or file "nil-context"))
        recording-path (str (fs/path rec-dir recording-filename))]
    
    (if (= file "nil.context")
      (do
        (println "Starting Amazon Q without any context file")
        (when (asciinema-available?)
          (println (format "Recording session to: %s" recording-path))))
      (do
        (println (format "Starting Amazon Q with context file: %s" file))
        (println (format "Full path: %s" full-path))))
    
    (when (asciinema-available?)
      (println (format "Recording session to: %s" recording-path)))
    
    (println "Type your queries after Amazon Q starts...")
    (println)
    
    ;; Check if expect is available
    (if (expect-available?)
      ;; Use expect to automate the initial command with asciinema recording
      (let [temp-script (fs/create-temp-file {:prefix "q-context-" :suffix ".exp"})
            temp-script-path (str temp-script)
            script-content (cond
                            ;; nil.context with asciinema
                            (and (= file "nil.context") (asciinema-available?))
                            (format "#!/usr/bin/env expect\n\nspawn asciinema rec %s -c \"q chat\"\ninteract\n" 
                                    recording-path)
                            ;; nil.context without asciinema
                            (= file "nil.context")
                            "#!/usr/bin/env expect\n\nspawn q chat\ninteract\n"
                            ;; regular context file with asciinema
                            (asciinema-available?)
                            (format "#!/usr/bin/env expect\n\nspawn asciinema rec %s -c \"q chat\"\nexpect \">\"\nsend \"refer %s\\r\"\ninteract\n" 
                                    recording-path full-path)
                            ;; regular context file without asciinema
                            :else
                            (format "#!/usr/bin/env expect\n\nspawn q chat\nexpect \">\"\nsend \"refer %s\\r\"\ninteract\n" 
                                    full-path))]
        
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
            script-content (cond
                            ;; nil.context with asciinema
                            (and (= file "nil.context") (asciinema-available?))
                            (format "#!/bin/bash\n\n# Start q chat with asciinema recording\necho \"Starting Amazon Q with recording (no context file)...\"\nasciinema rec %s -c \"q chat\"\n" 
                                    recording-path)
                            ;; nil.context without asciinema
                            (= file "nil.context")
                            "#!/bin/bash\n\n# Start q chat\necho \"Starting Amazon Q (no context file)...\"\nq chat\n"
                            ;; regular context file with asciinema
                            (asciinema-available?)
                            (format "#!/bin/bash\n\n# Start q chat with asciinema recording\necho \"Starting Amazon Q with recording...\"\necho \"When the prompt appears, type: refer %s\"\nasciinema rec %s -c \"q chat\"\n" 
                                    full-path recording-path)
                            ;; regular context file without asciinema
                            :else
                            (format "#!/bin/bash\n\n# Start q chat\necho \"Starting Amazon Q...\"\necho \"When the prompt appears, type: refer %s\"\nq chat\n" 
                                    full-path))]
        
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
  
  (let [context-dir (if (seq args) (first args) "~/q")
        files (list-context-files context-dir)]
    (println (format "Using context directory: %s" context-dir))
    (if (seq files)
      (if-let [selected (select-file-with-gum files)]
        (run-q-with-context context-dir selected)
        (println "Selection cancelled or no file selected."))
      (do
        (println (format "No .context files found in %s" context-dir))
        (println "Would you like to start Amazon Q without any context file? (y/n)")
        (flush)
        (let [input (str/lower-case (str/trim (read-line)))]
          (if (= input "y")
            (run-q-with-context context-dir "nil.context")
            (println "Cancelled.")))))))

(defn -main [& args]
  (apply q-context args))
