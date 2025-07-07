(ns knock.qtmux
  (:require [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(defn get-current-timestamp []
  "Generate timestamp in format YYYY-MM-DD-HH-MM-SS"
  (.format (LocalDateTime/now) 
           (DateTimeFormatter/ofPattern "yyyy-MM-dd-HH-mm-ss")))

(defn list-tmux-sessions []
  "Get list of all tmux sessions"
  (try
    (let [result (p/shell {:out :string :continue true} "tmux" "list-sessions" "-F" "#{session_name}")]
      (if (zero? (:exit result))
        (->> (:out result)
             str/split-lines
             (remove str/blank?)
             vec)
        []))
    (catch Exception e
      (println "Error listing tmux sessions:" (.getMessage e))
      [])))

(defn select-tmux-session [sessions]
  "Use gum to select a tmux session from the list"
  (when (seq sessions)
    (let [temp-file (java.io.File/createTempFile "tmux-sessions" ".txt")]
      (try
        ;; Write sessions to temp file
        (spit temp-file (str/join "\n" sessions))
        
        ;; Use gum filter to select session
        (let [result (p/shell {:out :string :continue true}
                             "bash" "-c" 
                             (str "cat " (.getAbsolutePath temp-file) " | gum filter --height 10 --placeholder 'Select tmux session...'"))]
          (when (zero? (:exit result))
            (str/trim (:out result))))
        (finally
          (.delete temp-file))))))

(defn ensure-rec-directory []
  "Ensure ~/rec directory exists"
  (let [rec-dir (io/file (System/getProperty "user.home") "rec")]
    (when-not (.exists rec-dir)
      (.mkdirs rec-dir))
    rec-dir))

(defn start-asciinema-recording [session-name]
  "Start asciinema recording for the tmux session"
  (let [rec-dir (ensure-rec-directory)
        timestamp (get-current-timestamp)
        cast-file (io/file rec-dir (str "tmux-" session-name "-" timestamp ".cast"))]
    (println (str "Starting asciinema recording: " (.getAbsolutePath cast-file)))
    (.getAbsolutePath cast-file)))

(defn set-shell-title [title]
  "Set the shell window title, removing any environment suffixes"
  (try
    ;; Clean the title by removing any environment indicators
    (let [clean-title (-> title
                          str/trim
                          ;; Remove any parenthetical content
                          (str/replace #"\s*\([^)]*\)" "")
                          str/trim)]
      
      ;; Set the clean title using ANSI escape sequences
      (print (str "\033]0;" clean-title "\007"))
      (flush)
      
      (println (str "Set terminal title to: '" clean-title "'")))
    (catch Exception e
      (println "Warning: Could not set shell title:" (.getMessage e)))))

(defn attach-to-session-with-recording [session-name cast-file]
  "Attach to tmux session with asciinema recording"
  (try
    (println (str "Attaching to tmux session: " session-name))
    (println (str "Recording to: " cast-file))
    
    ;; Set shell title to session name
    (set-shell-title session-name)
    
    ;; Use asciinema rec to record the tmux attach command
    (p/shell "asciinema" "rec" cast-file "-c" (str "tmux attach-session -t " session-name))
    
    (println "Recording completed.")
    (catch Exception e
      (println "Error during recording/attach:" (.getMessage e)))))

(defn -main [& args]
  (println "🎬 Q-Tmux: Select and record tmux sessions")
  
  ;; Check if tmux is available
  (let [tmux-check (p/shell {:out :string :continue true} "which" "tmux")]
    (when-not (zero? (:exit tmux-check))
      (println "❌ Error: tmux is not installed or not in PATH")
      (System/exit 1)))
  
  ;; Check if asciinema is available
  (let [asciinema-check (p/shell {:out :string :continue true} "which" "asciinema")]
    (when-not (zero? (:exit asciinema-check))
      (println "❌ Error: asciinema is not installed or not in PATH")
      (System/exit 1)))
  
  ;; Check if gum is available
  (let [gum-check (p/shell {:out :string :continue true} "which" "gum")]
    (when-not (zero? (:exit gum-check))
      (println "❌ Error: gum is not installed or not in PATH")
      (System/exit 1)))
  
  ;; Get tmux sessions
  (let [sessions (list-tmux-sessions)]
    (if (empty? sessions)
      (println "📭 No tmux sessions found. Create a session first with: tmux new-session -s <session-name>")
      (do
        (println (str "📋 Found " (count sessions) " tmux session(s)"))
        
        ;; Let user select session
        (if-let [selected-session (select-tmux-session sessions)]
          (do
            (println (str "✅ Selected session: " selected-session))
            
            ;; Start recording and attach
            (let [cast-file (start-asciinema-recording selected-session)]
              (attach-to-session-with-recording selected-session cast-file)))
          (println "❌ No session selected. Exiting."))))))
