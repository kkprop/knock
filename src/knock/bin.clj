(ns knock.bin
  (:require [babashka.process :as p]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [knock.roam :as roam])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]
           [java.util.concurrent Executors ScheduledExecutorService TimeUnit]))

(def ^:private clipboard-history (atom []))
(def ^:private last-clipboard-content (atom nil))
(def ^:private watcher-running (atom false))
(def ^:private executor (atom nil))
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

(defn- add-to-history [content]
  (when (and content 
             (not (str/blank? content))
             (not= content @last-clipboard-content))
    (reset! last-clipboard-content content)
    (let [item (create-bin-item content)]
      (swap! clipboard-history conj item)
      (save-history))))

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

(defn- handle-item-action [item action]
  (case action
    :roam (do
            (try
              (println "📝 Sending to Roam...")
              (let [g (roam/personal)  ; Load personal Roam graph config
                    content (:content item)
                    summary (:summary item)
                    timestamp (:exact-time item)
                    ; Format content for Roam with metadata
                    roam-content (str "**Clipboard:** " summary "\n"
                                     "**Time:** " timestamp "\n"
                                     "**Content:**\n"
                                     content)]
                ; Send to Roam using existing write function
                (roam/write g roam-content)
                (println "✅ Successfully sent to Roam!")
                
                ; Mark item as sent to Roam and update history
                (let [updated-item (assoc item :roam-sent true :roam-sent-at (get-current-time))]
                  (swap! clipboard-history 
                         (fn [history]
                           (mapv #(if (= (:id %) (:id item))
                                    updated-item
                                    %)
                                 history)))
                  (save-history)
                  (println "📝 Item marked as sent to Roam")
                  {:action :roam :item updated-item :success true}))
              (catch Exception e
                (println "❌ Failed to send to Roam:" (.getMessage e))
                {:action :roam :item item :success false :error (.getMessage e)})))
    
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
  (print "\033[2J\033[H")
  (flush)
  (println "📄 Selected Item Details")
  (println "========================")
  (println (str "📄 Summary: " (:summary item)))
  (println (str "📝 Created at: " (or (:exact-time item) (:hour item) "Unknown")))
  (println (str "📏 Length: " (:length item) " characters"))
  (when (:roam-sent item)
    (println "✅ Already sent to Roam"))
  (println "\n📄 Full Content:")
  (println "----------------------------------------")
  (println (:content item))
  (println "----------------------------------------")
  (println "")
  
  ; Automatically send to Roam
  (if (:roam-sent item)
    (do
      (println "⚠️  This item was already sent to Roam.")
      (println "Press Enter to continue...")
      (read-line)
      :cancel)
    (do
      (println "📝 Sending to Roam...")
      :roam))
  
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
  )

(defn- show-clipboard-board []
  (let [items @clipboard-history]
    (if (empty? items)
      (do
        (print "\033[2J\033[H") ; Clear screen and reset cursor
        (flush)
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
                                      (when (:roam-sent item) " ✅"))))))]
        
        (if (empty? gum-items)
          (do
            (println "No items to display")
            (Thread/sleep 2000))
          (try
            (print "\033[2J\033[H") ; Clear screen and reset cursor
            (flush)
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
                                 "--height" "20"
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
                            (when-let [action (show-item-actions selected-item)]
                              (handle-item-action selected-item action))
                            (catch Exception e
                              (println "ERROR in show-item-actions:" (.getMessage e))
                              (.printStackTrace e))))))))
                (do
                  ; Clean up temp file
                  (try (.delete (io/file temp-file)) (catch Exception _))
                  (Thread/sleep 2000))))
            
            (catch Exception e
              (println "Error in display:" (.getMessage e))
              (println "Stack trace:")
              (.printStackTrace e)
              (println "Retrying in 3 seconds...")
              (Thread/sleep 3000))))))))

(defn- interactive-board-loop []
  (loop []
    (try
      ; Show the board
      (show-clipboard-board)
      (catch Exception e
        (println "Loop error:" (.getMessage e))
        (Thread/sleep 2000)))
    
    ; Wait before showing again
    (Thread/sleep 1000)
    (recur)))

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
