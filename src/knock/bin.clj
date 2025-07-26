(ns knock.bin
  (:require [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [knock.roam :as roam]
            [knock.utils :as utils])
  (:import [java.util.concurrent Executors TimeUnit]))

(def ^:private clipboard-history (atom []))
(def ^:private last-clipboard-content (atom nil))
(def ^:private watcher-running (atom false))
(def ^:private executor (atom nil))
(def ^:private last-history-count (atom 0))
(def ^:private board-needs-refresh (atom true))
(def ^:private history-file (str (System/getProperty "user.home") "/.knock-clipboard-history.json"))

;; ===== OPTIMIZED CLIPBOARD LOADING SOLUTION =====
;; Cache for recent items only - avoids repeated file parsing
(def ^:private recent-items-cache (atom {:items [] :last-modified 0 :size 50}))

(defn- invalidate-cache []
  "Force cache invalidation for immediate UI updates"
  (reset! recent-items-cache {:items [] :last-modified 0 :size 50}))

(defn- get-recent-items-fast [n]
  "Get recent N items with smart caching - only reloads if file changed"
  (let [file (io/file history-file)]
    (if (.exists file)
      (let [current-modified (.lastModified file)
            cached @recent-items-cache]
        (if (or (> current-modified (:last-modified cached))
                (not= n (:size cached))
                (empty? (:items cached)))
          ;; File changed or cache miss - reload recent items only
          (let [file-content (slurp history-file)
                all-items (json/read-str file-content :key-fn keyword)
                ;; Only process the recent N items, not all 2047+
                recent-items (take-last n all-items)
                processed-items (mapv #(-> %
                                          (update :create-time (fn [time-str] (when time-str time-str)))
                                          (update :roam-sent-at (fn [time-str] (when time-str time-str))))
                                    recent-items)]
            (reset! recent-items-cache {:items processed-items 
                                       :last-modified current-modified
                                       :size n})
            processed-items)
          ;; Use cached items - instant return
          (:items cached)))
      [])))

(defn- load-history []
  "Optimized history loading - only loads recent items into main atom"
  (try
    ;; Load only recent 100 items into the main clipboard-history atom
    ;; This maintains compatibility with existing code
    (let [recent-items (get-recent-items-fast 100)]
      (reset! clipboard-history recent-items))
    (catch Exception e
      (println "Warning: Could not load clipboard history:" (.getMessage e)))))

(defn- save-history []
  (try
    (let [data (mapv #(-> %
                         (update :create-time str) ; Already string, just ensure
                         (update :roam-sent-at 
                                 (fn [time] (when time (str time)))))
                     @clipboard-history)]
      (spit history-file (json/write-str data {:pretty true})))
    (catch Exception e
      (println "Warning: Could not save clipboard history:" (.getMessage e)))))

(defn- get-current-time []
  (utils/cur-time-str))

(defn- format-time-exact 
  "Keep exact time in metadata (HH:mm)"
  [datetime]
  (utils/cur-time-str "HH:mm"))

(defn- format-time-for-display 
  "Display time with hour granularity (HH:00)"
  [datetime]
  (utils/cur-time-str "HH:00"))

(defn- format-date [datetime]
  (utils/cur-date-str))

(defn- get-clipboard-content []
  (try
    (let [result (p/shell {:out :string :err :string :continue true} "pbpaste")]
      (when (zero? (:exit result))
        (let [content (str/trim (:out result))]
          (when-not (str/blank? content)
            content))))
    (catch Exception _
      nil)))

(defn- generate-summary [content]
  (let [lines (str/split-lines content)
        first-line (first lines)
        summary (if (> (count first-line) 60)
                  (str (subs first-line 0 57) "...")
                  first-line)]
    (str/replace summary #"\s+" " ")))

(defn- create-bin-item [content]
  (let [now (get-current-time)
        exact-time (format-time-exact now)
        display-hour (format-time-for-display now)
        date (format-date now)]
    {:id (str (System/currentTimeMillis))
     :content content
     :length (count content)
     :create-time now
     :summary (generate-summary content)
     :exact-time exact-time
     :display-hour display-hour
     :date date}))

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
  "Group items by date and display hour, with most recent first"
  (->> items
       (group-by (fn [item] [(:date item) (:display-hour item)]))
       (sort-by (fn [[[date hour] _]] [date hour]))
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
    (str/includes? content "摘录来自") (-> content
                                        (str/split #"摘录来自\n")
                                        first
                                        str/trim)
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
  (let [log-file "/tmp/roam-sending-debug.log"
        log-msg (fn [msg] 
                  (spit log-file 
                        (str (get-current-time) " - " msg "\n") 
                        :append true))]
    (case action
      :roam (do
              (log-msg (str "START: Async Roam sending for item ID: " (:id item)))
              ; Immediately mark as sending and update UI
              (let [sending-item (assoc item :roam-sending true)]
                (log-msg "STEP 1: Marked item as sending")
                (swap! clipboard-history 
                       (fn [history]
                         (mapv #(if (= (:id %) (:id item))
                                  sending-item
                                  %)
                               history)))
                (save-history)
                (invalidate-cache)  ; Force immediate UI refresh
                (log-msg "STEP 2: Updated clipboard history and saved")
                
                ; Send to Roam asynchronously using go! for fluent UI
                (utils/go!
                  (try
                    (log-msg "STEP 3: Starting async Roam connection")
                    (let [g (roam/personal)]
                      (log-msg "STEP 4: Got Roam connection")
                      (let [content (:content item)
                            
                            ; Apply content decoration
                            decorated-content (decorate-content content)
                            
                            ; Get target block (work vs personal)
                            target-block (get-target-block g content)
                            
                            ; Format without timestamp
                            roam-content decorated-content]
                        
                        (log-msg (str "STEP 5: Prepared content - original: '" content "', decorated: '" decorated-content "', target: " target-block))
                        
                        ; Send to Roam using the same pattern as pb->roam
                        (if (is-ip-address? content)
                          (do
                            (log-msg "STEP 6a: Sending IP address to Work block")
                            (roam/write g roam-content :page target-block :order "first"))
                          (do
                            (log-msg "STEP 6b: Sending regular content to Personal block")
                            (roam/write g roam-content :page target-block)))
                        
                        (log-msg "STEP 7: Successfully sent to Roam")
                        
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
                        (invalidate-cache)  ; Force cache refresh for immediate UI update
                        ; Trigger UI refresh by killing gum process to restart display
                        (try
                          (utils/kill-cur-pid-by-name "gum")
                          (catch Exception _))
                        (log-msg "STEP 8: Marked item as successfully sent and saved")))
                    (catch Exception e
                      (log-msg (str "ERROR: Exception during async Roam sending: " (.getMessage e)))
                      (log-msg (str "ERROR: Stack trace: " (str/join "\n" (.getStackTrace e))))
                      ; Mark as failed
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
                      (invalidate-cache)  ; Force cache refresh for immediate UI update
                      ; Trigger UI refresh by killing gum process to restart display
                      (try
                        (utils/kill-cur-pid-by-name "gum")
                        (catch Exception _))
                      (log-msg "STEP 9: Marked item as failed and saved")))
                  (log-msg "END: Async Roam sending process completed"))
                
                {:action :roam :item sending-item :success true :async true})))
    
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

(defn- reset-terminal []
  "Minimal terminal reset that doesn't disrupt shell"
  (print "\033[2J\033[H\033[0m")  ; Clear screen, home cursor, reset attributes
  (flush)
  (Thread/sleep 100))

(defn- create-indexed-display-fast [recent-items]
  "Create indexed display for O(1) selection lookup"
  (let [grouped (group-by-hour recent-items)
        item-index (atom {})
        display-lines (atom [])]
    
    ;; Build display and index in single pass
    (doseq [[[date hour] hour-items] grouped]
      (let [today (format-date (get-current-time))
            display-header (if (= date today)
                            (str "⏰ Today " hour)
                            (str "⏰ " date " " hour))]
        (swap! display-lines conj display-header)
        
        ;; Add items with index mapping
        (doseq [item (reverse hour-items)]
          (let [display-line (str "    • " (:summary item) " (" (:length item) " chars) [" (:exact-time item) "]"
                                (cond
                                  (:roam-sent item) " ✅"
                                  (:roam-sending item) " 🔄"
                                  (:roam-send-failed item) " ❌"))]
            (swap! item-index assoc (str/trim display-line) item)  ; Index with trimmed line
            (swap! display-lines conj display-line)))))  ; Close inner doseq and outer let and outer doseq
    
    {:display-lines @display-lines
     :item-index @item-index}))

(defn- show-clipboard-board []
  "Optimized clipboard board with instant loading of recent items - loops until exit"
  (loop [continue-loop true]
    (when (and continue-loop (not @user-exit-requested))
      (let [;; Use live clipboard-history atom for immediate UI updates - FAST!
            recent-items (take-last 50 @clipboard-history)
            total-count (if (.exists (io/file history-file))
                         ;; Quick count without parsing entire file
                         (let [content (slurp history-file)
                               items (json/read-str content :key-fn keyword)]
                           (count items))
                         0)]
        
        (if (empty? recent-items)
          (do
            (println "📋 Knock Clipboard Manager")
            (println "==========================")
            (println "🔍 Clipboard watcher active")
            (println "💡 Copy text to see it appear below")
            (println "")
            (println "📋 No clipboard history yet. Copy something to get started!")
            (println "Press Ctrl+C to exit")
            (reset-terminal)
            (Thread/sleep 3000)
            (recur true))  ; Continue looping
          
          ;; Create indexed display for instant selection
          (let [{:keys [display-lines item-index]} (create-indexed-display-fast recent-items)]
            
            (if (empty? display-lines)
              (do
                (println "No items to display")
                (Thread/sleep 2000)
                (recur true))  ; Continue looping
              
              ;; Show the gum interface
              (let [should-continue
                    (try
                      ;; Ensure clean terminal state before showing content
                      (Thread/sleep 100)  ; Brief pause to ensure previous processes are done
                      (print "\033[2J\033[H\033[0m")  ; Clear screen, home cursor, reset attributes
                      (flush)
                      (Thread/sleep 50)   ; Allow terminal to process the reset
                      (println "📋 Knock Clipboard Manager")
                      (println "==========================")
                      (println "🔍 Clipboard watcher active")
                      (println "💡 Copy text to see it appear below")
                      (println (str "📊 Showing recent 50 items (total: " total-count ")"))
                      (println "")
                      
                      ;; Use gum choose with indexed display
                      (let [temp-file (str "/tmp/gum-selection-" (System/currentTimeMillis))
                            result (p/shell {:in (str/join "\n" display-lines)
                                            :out temp-file
                                            :continue true}
                                           "gum" "choose" 
                                           "--height" "30"
                                           "--header" "📋 Recent Clipboard Items")]
                        (if (zero? (:exit result))
                          (let [selected-line (str/trim (slurp temp-file))
                                log-file "/tmp/roam-sending-debug.log"
                                log-msg (fn [msg] 
                                          (spit log-file 
                                                (str (get-current-time) " - " msg "\n") 
                                                :append true))]
                            (log-msg (str "GUM: Selection made - selected-line: '" selected-line "'"))
                            ;; Clean up temp file
                            (try (.delete (io/file temp-file)) (catch Exception _))
                            ;; Reset terminal state after gum choose exits
                            (print "\033[0m")
                            (flush)
                            (when-not (str/blank? selected-line)
                              (log-msg "GUM: Selected line is not blank")
                              ;; Skip hour headers (lines starting with ⏰)
                              (if (str/starts-with? selected-line "⏰")
                                (log-msg "GUM: Skipping hour header")
                                (do
                                  (log-msg "GUM: Processing item selection")
                                  ;; INSTANT LOOKUP - O(1) instead of linear search!
                                  (if-let [selected-item (get item-index selected-line)]
                                    (do
                                      (log-msg (str "GUM: Found item in index - ID: " (:id selected-item)))
                                      (log-msg (str "SELECTION: Item selected - ID: " (:id selected-item) ", content: '" (:content selected-item) "'"))
                                      (log-msg (str "SELECTION: Item status - roam-sent: " (:roam-sent selected-item) ", roam-sending: " (:roam-sending selected-item)))
                                      (try
                                        ;; Send to Roam if item hasn't been sent yet and isn't currently sending
                                        (if (or (:roam-sent selected-item) 
                                                (:roam-sending selected-item))
                                          (log-msg "SELECTION: Item already sent or sending - skipping")
                                          (do
                                            (log-msg "SELECTION: Item eligible for Roam sending - calling handle-item-action")
                                            (handle-item-action selected-item :roam)))
                                        (catch Exception e
                                          (log-msg (str "SELECTION ERROR: " (.getMessage e)))
                                          (println "ERROR in item selection:" (.getMessage e))
                                          (.printStackTrace e))))
                                    (log-msg "GUM: Item not found in index")))))
                            ;; Continue looping after processing selection
                            true)
                          ;; Handle gum exit scenarios
                          (do
                            (try (.delete (io/file temp-file)) (catch Exception _))
                            (print "\033[0m")
                            (flush)
                            (cond
                              (= 130 (:exit result))
                              (do
                                (reset! user-exit-requested true)
                                (reset-terminal)
                                (println "📋 Clipboard Manager stopped by user.")
                                (System/exit 0))
                              (= 143 (:exit result))
                              (do
                                (reset-terminal)
                                (println "Refreshing with new clipboard content...")
                                (Thread/sleep 500)
                                true)  ; Continue looping after refresh
                              (or (= 1 (:exit result)) (= 2 (:exit result)))
                              true  ; User cancelled, continue looping
                              :else
                              (do
                                (Thread/sleep 1000)
                                true))))) ; Continue looping for any other exit codes
                      (catch Exception e
                        (println "Error in clipboard board:" (.getMessage e))
                        (Thread/sleep 1000)
                        true))] ; Continue looping on error
                
                (recur should-continue))))))))) ; Use the flag to determine if we should continue

(defn- interactive-board-loop []
  ;; show-clipboard-board now handles its own looping, so just call it once
  (show-clipboard-board))

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
