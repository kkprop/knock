(ns knock.bin
  (:require [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [knock.roam :as roam]
            [knock.utils :as utils])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]
           [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

(def ^:private clipboard-history (atom []))
(def ^:private last-clipboard-content (atom nil))
(def ^:private watcher-running (atom false))
(def ^:private executor (atom nil))
(def ^:private last-history-count (atom 0))
(def ^:private board-needs-refresh (atom true))
(def ^:private history-file (str (System/getProperty "user.home") "/.knock-clipboard-history.json"))

(defn- load-history []
  (try
    (when (.exists (io/file history-file))
      (let [data (json/read-str (slurp history-file) :key-fn keyword)]
        (reset! clipboard-history 
                (mapv #(-> %
                          (update :create-time 
                                  (fn [time-str] 
                                    (LocalDateTime/parse time-str)))
                          (update :roam-sent-at 
                                  (fn [time-str] 
                                    (when time-str (LocalDateTime/parse time-str)))))
                      data))))
    (catch Exception e
      (println "Warning: Could not load clipboard history:" (.getMessage e)))))

(defn- save-history []
  (try
    (let [data (mapv #(-> %
                         (update :create-time str)
                         (update :roam-sent-at 
                                 (fn [time] (when time (str time)))))
                     @clipboard-history)]
      (spit history-file (json/write-str data {:pretty true})))
    (catch Exception e
      (println "Warning: Could not save clipboard history:" (.getMessage e)))))

(defn- get-current-time []
  (LocalDateTime/now))

(defn- format-time-exact [datetime]
  "Keep exact time in metadata (HH:mm)"
  (.format datetime (DateTimeFormatter/ofPattern "HH:mm")))

(defn- format-time-for-display [datetime]
  "Display time with hour granularity (HH:00)"
  (.format datetime (DateTimeFormatter/ofPattern "HH:00")))

(defn- format-date [datetime]
  (.format datetime (DateTimeFormatter/ofPattern "yyyy-MM-dd")))

(defn- get-clipboard-content []
  (try
    (let [result (p/shell {:out :string :err :string :continue true} "pbpaste")]
      (when (zero? (:exit result))
        (let [content (str/trim (:out result))]
          (when-not (str/blank? content)
            content))))
    (catch Exception e
      nil)))

(defn- generate-summary [content]
  (let [lines (str/split-lines content)
        first-line (first lines)
        summary (if (> (count first-line) 60)
                  (str (subs first-line 0 57) "...")
                  first-line)]
    (str/replace summary #"\s+" " ")))

(defn- create-bin-item [content]
  (let [now (get-current-time)]
    {:id (str (System/currentTimeMillis))
     :content content
     :length (count content)
     :create-time now
     :summary (generate-summary content)
     :exact-time (format-time-exact now)  ; Keep exact time
     :display-hour (format-time-for-display now)  ; Hour for display
     :date (format-date now)}))

(def ^:private user-exit-requested (atom false))

(defn- add-to-history [content]
  (when (and content 
             (not (str/blank? content))
             (not= content @last-clipboard-content))
    (reset! last-clipboard-content content)
    (let [item (create-bin-item content)]
      (swap! clipboard-history conj item)
      (save-history)
      ; Trigger board refresh when new content is added
      (reset! board-needs-refresh true)
      ; Kill any running gum processes to refresh the display immediately
      ; Use SIGTERM instead of SIGKILL for cleaner termination
      (try
        (utils/kill-cur-pid-by-name "gum")
        (catch Exception _)))))

(defn- group-by-hour [items]
  "Group items by display hour (HH:00) while preserving exact timestamps"
  (->> items
       (group-by :display-hour)
       (sort-by key)
       reverse))

(defn- save-discarded-item [item]
  (let [bin-dir (io/file (System/getProperty "user.home") "bin")
        filename (str (:id item) ".json")]
    (.mkdirs bin-dir)
    (let [file-path (io/file bin-dir filename)
          item-data (assoc item :discarded-at (get-current-time))]
      (spit file-path (json/write-str item-data {:pretty true}))
      (println (str "🗑️  Saved to ~/bin/" filename)))))

(defn- should-send-to-roam? [content prev-items]
  "Apply pb->roam filtering logic to determine if content should be sent"
  (and (not (str/blank? content))
       (not (str/includes? content "-----BEGIN CERTIFICATE-----"))
       (not (str/includes? content "-----BEGIN CERTIFICATE REQUEST-----"))
       (not (str/includes? content "-----BEGIN RSA PRIVATE KEY-----"))
       (not (some #(= content %) prev-items))  ; Check against previous items
       (not (re-matches #"^\d+$" content))     ; Not just digits
       (not (> (count content) 1000))          ; Not too long
       ))

(defn- decorate-content [content]
  "Apply pb->roam content decoration logic"
  (cond
    (str/includes? content "::") (str "`" content "`")
    (str/includes? content "摘录来自") content  ; Would need epub-clean function
    :else content))

(defn- is-ip-address? [s]
  "Simple IP address detection"
  (re-matches #"^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}$" s))

(defn- get-target-block [g content]
  "Determine target block based on content type (following pb->roam logic)"
  (try
    (if (is-ip-address? content)
      (utils/mock-within 1800 roam/daily-note-block g "#Work")    ; Work block for IPs
      (utils/mock-within 1800 roam/daily-note-block g "#Personal")) ; Personal block for others
    (catch Exception e
      ; Fallback to daily page if block lookup fails
      (roam/cur-daily-page))))

(defn- handle-item-action [item action]
  (case action
    :roam (do
            ; Immediately mark as sending and update UI
            (let [sending-item (assoc item :roam-sending true)]
              (swap! clipboard-history 
                     (fn [history]
                       (mapv #(if (= (:id %) (:id item))
                                sending-item
                                %)
                             history)))
              (save-history)
              (reset! board-needs-refresh true)
              (println "📝 Queued for Roam (sending in background)...")
              
              ; Send to Roam in background thread
              (future
                (try
                  (let [g (roam/personal)
                        content (:content item)
                        
                        ; Apply content decoration
                        decorated-content (decorate-content content)
                        
                        ; Get target block (work vs personal)
                        target-block (get-target-block g content)
                        
                        ; Format with timestamp like pb->roam
                        timestamp (:exact-time item)
                        roam-content (str decorated-content 
                                         (when timestamp (str " `" timestamp "`")))]
                    
                    ; Send to Roam using the same pattern as pb->roam
                    (if (is-ip-address? content)
                      (roam/write g roam-content :page target-block :order "first")
                      (roam/write g roam-content :page target-block))
                    
                    (println "✅ Successfully sent to Roam!")
                    
                    ; Mark item as successfully sent to Roam
                    (swap! clipboard-history 
                           (fn [history]
                             (mapv #(if (= (:id %) (:id item))
                                      (-> %
                                          (dissoc :roam-sending)
                                          (assoc :roam-sent true 
                                                 :roam-sent-at (get-current-time)))
                                      %)
                                   history)))
                    (save-history)
                    (reset! board-needs-refresh true)
                    (println "📝 Item marked as sent to Roam"))
                  (catch Exception e
                    (println "❌ Failed to send to Roam:" (.getMessage e))
                    ; Mark as failed (no user interaction needed)
                    (swap! clipboard-history 
                           (fn [history]
                             (mapv #(if (= (:id %) (:id item))
                                      (-> %
                                          (dissoc :roam-sending)
                                          (assoc :roam-send-failed true
                                                 :roam-error (.getMessage e)))
                                      %)
                                   history)))
                    (save-history)
                    (reset! board-needs-refresh true))))
              
              {:action :roam :item sending-item :success true :async true}))
    
    ;; Future actions (commented out for now)
    ;; :discard (do
    ;;            (save-discarded-item item)
    ;;            (swap! clipboard-history #(remove (fn [i] (= (:id i) (:id item))) %))
    ;;            (save-history)
    ;;            {:action :discard :item item})
    ;; 
    ;; :copy (do
    ;;         (p/shell {:in (:content item)} "pbcopy")
    ;;         (println "📋 Copied to clipboard")
    ;;         {:action :copy :item item})
    
    :cancel {:action :cancel :item item}
    
    {:action :unknown :item item}))

(defn- show-item-actions [item]
  ; Skip action selection - directly send to Roam
  (print "\033[2J\033[H\033[0m") ; Enhanced screen clearing
  (flush)
  (Thread/sleep 100) ; Brief pause to ensure terminal reset
  (println "📄 Selected Item Details")
  (println "========================")
  (println (str "📄 Summary: " (:summary item)))
  (println (str "📝 Created at: " (or (:exact-time item) (:hour item) "Unknown")))
  (println (str "📏 Length: " (:length item) " characters"))
  (cond
    (:roam-sent item) (println "✅ Already sent to Roam")
    (:roam-sending item) (println "🔄 Currently sending to Roam...")
    (:roam-send-failed item) (println (str "❌ Failed to send to Roam: " (:roam-error item))))
  (println "\n📄 Full Content:")
  (println "----------------------------------------")
  (println (:content item))
  (println "----------------------------------------")
  (println "")
  
  ; Handle different states
  (cond
    (:roam-sent item)
    (do
      (println "⚠️  This item was already sent to Roam.")
      (println "Press Enter to continue...")
      (read-line)
      :cancel)
    
    (:roam-sending item)
    (do
      (println "🔄 This item is currently being sent to Roam...")
      (println "Press Enter to continue...")
      (read-line)
      :cancel)
    
    (:roam-send-failed item)
    (do
      (println "🔄 Retrying send to Roam...")
      :roam)  ; Automatically retry without user prompt
    
    :else
    (do
      (println "📝 Sending to Roam...")
      :roam)))
  
  ;; Future action options (commented out for now)
  ;; (let [actions ["📝 Write to Roam" "🗑️  Discard" "📋 Copy to Clipboard" "❌ Cancel"]]
  ;;   (println "Choose an action:")
  ;;   (doseq [[idx action] (map-indexed vector actions)]
  ;;     (println (str (inc idx) ". " action)))
  ;;   (println "")
  ;;   (print "Enter choice (1-4): ")
  ;;   (flush)
  ;;   
  ;;   (try
  ;;     (let [choice (read-line)
  ;;           choice-num (Integer/parseInt (str/trim choice))]
  ;;       (case choice-num
  ;;         1 :roam
  ;;         2 :discard
  ;;         3 :copy
  ;;         4 :cancel
  ;;         :cancel))
  ;;     (catch Exception e
  ;;       (println "Invalid choice, cancelling...")
  ;;       :cancel)))

(defn- reset-terminal []
  "Comprehensive terminal reset to fix formatting issues"
  ; Multiple reset sequences to ensure clean state
  (print "\033c")           ; Full terminal reset
  (print "\033[2J")         ; Clear screen
  (print "\033[H")          ; Home cursor
  (print "\033[0m")         ; Reset all attributes
  (print "\033[?25h")       ; Show cursor
  (flush)
  (Thread/sleep 200))       ; Longer pause for terminal to stabilize

(defn- show-clipboard-board []
  (let [items @clipboard-history]
    (if (empty? items)
      (do
        (reset-terminal)
        (println "📋 Knock Clipboard Manager")
        (println "==========================")
        (println "🔍 Clipboard watcher active")
        (println "💡 Copy text to see it appear below")
        (println "")
        (println "📋 No clipboard history yet. Copy something to get started!")
        (println "Press Ctrl+C to exit")
        (Thread/sleep 3000))
      (let [; Create timeline format for gum input
            grouped (group-by-hour items)
            ; Create timeline content with hour headers and indented items for gum
            gum-items (flatten
                       (for [[hour hour-items] grouped]
                         (cons (str "⏰ " hour)
                               (for [item (reverse hour-items)]
                                 (str "    • " (:summary item) " (" (:length item) " chars) [" (:exact-time item) "]"
                                      (cond
                                        (:roam-sent item) " ✅"
                                        (:roam-sending item) " 🔄"
                                        (:roam-send-failed item) " ❌"))))))]
        
        (if (empty? gum-items)
          (do
            (println "No items to display")
            (Thread/sleep 2000))
          (try
            (reset-terminal)
            (println "📋 Knock Clipboard Manager")
            (println "==========================")
            (println "🔍 Clipboard watcher active")
            (println "💡 Copy text to see it appear below")
            (when (> (count @clipboard-history) 50)
              (println (str "📊 Total items: " (count @clipboard-history))))
            (println "")
            
            ; Use timeline format in gum choose
            (let [temp-file (str "/tmp/gum-selection-" (System/currentTimeMillis))
                  result (p/shell {:in (str/join "\n" gum-items)
                                  :out temp-file
                                  :continue true}
                                 "gum" "choose" 
                                 "--height" "60"
                                 "--header" "📋 Clipboard Timeline")]
              (if (zero? (:exit result))
                (let [selected-line (str/trim (slurp temp-file))]
                  ; Clean up temp file
                  (try (.delete (io/file temp-file)) (catch Exception _))
                  (when-not (str/blank? selected-line)
                    ; Skip hour headers (lines starting with ⏰)
                    (when-not (str/starts-with? selected-line "⏰")
                      (let [; Extract summary from indented line (handle both 4-space and no-space formats)
                            selected-summary (-> selected-line
                                               (str/replace #"^    • " "") ; Remove 4-space indentation
                                               (str/replace #"^• " "")     ; Remove no-space format
                                               (str/split #" \(")
                                               first)
                            selected-item (->> @clipboard-history
                                             (filter #(= (:summary %) selected-summary))
                                             first)]
                        (when selected-item
                          (try
                            ; Immediately handle the action without showing details screen
                            (handle-item-action selected-item :roam)
                            ; Return immediately to main menu - no user interaction needed
                            (catch Exception e
                              (println "ERROR in handle-item-action:" (.getMessage e))
                              (.printStackTrace e))))))))
                (do
                  ; Clean up temp file
                  (try (.delete (io/file temp-file)) (catch Exception _))
                  ; Handle different exit scenarios
                  (cond
                    ; User pressed Ctrl+C (SIGINT) - exit the program
                    (= 130 (:exit result))
                    (do
                      (reset! user-exit-requested true)
                      (reset-terminal)
                      (println "📋 Clipboard Manager stopped by user.")
                      (System/exit 0))
                    
                    ; Process was killed programmatically (SIGTERM) - refresh
                    (= 143 (:exit result))
                    (do
                      (reset-terminal)
                      (println "Refreshing with new clipboard content...")
                      (Thread/sleep 500))
                    
                    ; User cancelled selection (ESC or no selection) - return to main menu immediately
                    (or (= 1 (:exit result)) (= 2 (:exit result)))
                    (do
                      ; Just return to main menu loop - no delay, no message
                      nil)
                    
                    ; Other exit codes - normal handling
                    :else
                    (Thread/sleep 1000)))))))))))

(defn- interactive-board-loop []
  (loop []
    (when-not @user-exit-requested
      (try
        ; Check if clipboard history has changed
        (let [current-count (count @clipboard-history)]
          (when (or @board-needs-refresh 
                    (not= current-count @last-history-count))
            (reset! last-history-count current-count)
            (reset! board-needs-refresh false)
            ; Show the board
            (show-clipboard-board)))
        (catch Exception e
          (println "Loop error:" (.getMessage e))
          (Thread/sleep 2000)))
      
      ; Wait before checking again
      (Thread/sleep 500)
      (recur))))

(defn- start-clipboard-watcher []
  (when-not @watcher-running
    (reset! watcher-running true)
    (let [scheduler (Executors/newScheduledThreadPool 1)]
      (reset! executor scheduler)
      
      ;; Initialize with current clipboard content
      (reset! last-clipboard-content (get-clipboard-content))
      
      (.scheduleAtFixedRate scheduler
                           (fn []
                             (try
                               (when @watcher-running
                                 (add-to-history (get-clipboard-content)))
                               (catch Exception e
                                 (when @watcher-running
                                   (println "Clipboard watcher error:" (.getMessage e))))))
                           1 1 TimeUnit/SECONDS)
      
      ;; Add shutdown hook
      (.addShutdownHook (Runtime/getRuntime)
                        (Thread. #(when @watcher-running
                                   (reset! watcher-running false)
                                   (when @executor
                                     (.shutdown @executor))))))))

(defn -main [& args]
  ; Add shutdown hook for proper terminal cleanup
  (.addShutdownHook (Runtime/getRuntime)
                    (Thread. (fn []
                              (reset-terminal)
                              (println "📋 Clipboard Manager stopped.")
                              (println "Terminal reset complete."))))
  
  (println "📋 Knock Clipboard Manager")
  (println "==========================")
  (println "🔍 Starting clipboard watcher...")
  (println "🎯 Interactive board ready!")
  (println "💡 Copy text to see it appear in timeline")
  (println "📝 Press Ctrl+C to exit")
  (println "")
  
  ;; Load existing history
  (load-history)
  
  ;; Start clipboard watcher
  (start-clipboard-watcher)
  
  ;; Start interactive board loop
  (interactive-board-loop))
