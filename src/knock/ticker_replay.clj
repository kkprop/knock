(ns knock.ticker-replay
  (:require [knock.utils :refer :all]
            [knock.tui :as tui]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell])
  (:import [java.io File]
           [java.util Date]
           [java.text SimpleDateFormat]))

;; Constants for display
(def ^:const header-format "%-6s %-8s %-20s %-10s %-10s %-10s %-10s %-10s %-10s %-10s %-10s")
(def ^:const row-format    "%-6s %-8s %-20s %-10s %-10s %-10s %-10s %-10s %-10s %-10s %-10s")

;; State management
(def state (atom {:files []
                  :current-index 0
                  :paused false
                  :speed 1.0
                  :last-frame nil
                  :current-frame nil
                  :display-rows 20
                  :last-update-time 0
                  :timestamp nil
                  :date-str nil
                  :debug-log []}))

;; Add debug log
(defn add-debug-log [msg]
  (swap! state update :debug-log conj msg))

;; Helper function to execute bash commands
(defn execute-bash [cmd]
  (let [result (shell/sh "bash" "-c" cmd)]
    (when (not= 0 (:exit result))
      (println "Error executing command:" cmd)
      (println "Error:" (:err result)))
    (:exit result)))

;; Helper function to get current volume from ticker data
(defn cur-volumn [s]
  (if (or (nil? s) (str/includes? s "nul") (empty? s))
    0
    (-> (if (and (not (empty? s)) (string? s) (not (empty? s)))
          (if (Character/isDigit (last s))
            s
            (chop-tail-n s 1))
          s)
        (precision :keep-digit 3)
        (parse-float))))

;; Format timestamp from Unix timestamp
(defn format-timestamp [ts]
  (when ts
    (try
      (let [date (Date. (* 1000 (long ts)))
            formatter (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")]
        (.format formatter date))
      (catch Exception e
        (str ts)))))

;; Get file list from date directory
(defn get-ticker-files [date-str]
  (let [dir (join-path "ticker" date-str)
        files (sort (ls-abs dir))]
    files))

;; Initialize the state with file list but don't load data yet
(defn initialize-state [date-str]
  (let [files (get-ticker-files date-str)]
    (swap! state assoc 
           :files files 
           :current-index 0
           :date-str date-str)
    (count files)))

;; Load ticker files from a specific date directory
(defn load-ticker-files [date-str]
  (let [dir (join-path "ticker" date-str)
        files (sort (ls-abs dir))]
    (swap! state assoc :files files :current-index 0)
    (count files)))

;; Calculate speed between two frames
(defn calculate-speed [prev-frame cur-frame]
  (when (and prev-frame cur-frame)
    (let [grouped-by-ticker (group-by :ticker (concat prev-frame cur-frame))]
      (->> grouped-by-ticker
           (map (fn [[ticker items]]
                  (when (= 2 (count items))
                    (let [prev (first items)
                          cur (second items)
                          prev-vol (cur-volumn (:volumn prev))
                          cur-vol (cur-volumn (:volumn cur))
                          speed (- cur-vol prev-vol)
                          multiplier (cond
                                       (and (string? (:volumn cur)) (str/ends-with? (:volumn cur) "M")) 1000
                                       (and (string? (:volumn cur)) (str/ends-with? (:volumn cur) "K")) 1
                                       :else (/ 1 1000.0))]
                      (assoc cur :speed (* multiplier speed))))))
           (filter identity)))))

;; Extract timestamp from filename
(defn extract-timestamp [file]
  (try
    (let [filename (file-name (basename file))
          timestamp (Long/parseLong filename)]
      timestamp)
    (catch Exception e
      nil)))

;; Load a specific frame by index - only when needed
(defn load-frame [index]
  (let [files (:files @state)
        file-count (count files)]
    (when (and (>= index 0) (< index file-count))
      (let [file (nth files index)
            timestamp (extract-timestamp file)
            frame (map #(assoc % :ts timestamp)
                       (slurp-ej-line file))]
        (swap! state assoc :timestamp timestamp)
        frame))))

;; Load current and previous frames on demand
(defn load-frames [index]
  (let [prev-index (max 0 (dec index))
        prev-frame (load-frame prev-index)
        cur-frame (load-frame index)]
    (swap! state assoc 
           :last-frame prev-frame
           :current-frame cur-frame
           :current-index index
           :last-update-time (System/currentTimeMillis))
    (calculate-speed prev-frame cur-frame)))

;; Toggle pause state
(defn toggle-pause []
  (swap! state update :paused not)
  (add-debug-log (str "Paused: " (:paused @state))))

;; Next frame
(defn next-frame []
  (when (< (:current-index @state) (dec (count (:files @state))))
    (load-frames (inc (:current-index @state)))
    (add-debug-log "Next frame")))

;; Previous frame
(defn prev-frame []
  (when (> (:current-index @state) 0)
    (load-frames (dec (:current-index @state)))
    (add-debug-log "Previous frame")))

;; Speed up
(defn speed-up []
  (swap! state update :speed #(min 10.0 (+ % 0.5)))
  (add-debug-log (str "Speed up: " (:speed @state))))

;; Speed down
(defn speed-down []
  (swap! state update :speed #(max 0.5 (- % 0.5)))
  (add-debug-log (str "Speed down: " (:speed @state))))

;; Format a ticker row for display
(defn format-ticker-row [{:keys [idx ticker ticker-name price change high low volumn range pe market-cap speed]}]
  (format row-format 
          (or idx "")
          (or ticker "")
          (or (when ticker-name (if (> (count ticker-name) 18) 
                                  (str (subs ticker-name 0 15) "...") 
                                  ticker-name)) "")
          (or price "")
          (or change "")
          (or high "")
          (or low "")
          (or volumn "")
          (or range "")
          (or pe "")
          (str (when speed (format "%.2f" speed)))))

;; Display header
(defn display-header []
  (println (format header-format 
                   "IDX" "TICKER" "NAME" "PRICE" "CHANGE" 
                   "HIGH" "LOW" "VOLUME" "RANGE" "PE" "SPEED")))

;; Display the current frame with calculated speeds
(defn display-frame [frame]
  (when frame
    (let [sorted-frame (reverse (sort-by :speed frame))
          display-rows (min (count sorted-frame) (:display-rows @state))
          timestamp (format-timestamp (:timestamp @state))]
      (tui/clear-screen)
      
      ;; Display navigation info with timestamp in the top bar
      (println "Ticker Replay - Date:" (get-in @state [:date-str] "Unknown") 
               "- Time:" timestamp)
      (println (str "Frame: " (:current-index @state) "/" (dec (count (:files @state))) 
                    " | Status: " (if (:paused @state) "PAUSED" "PLAYING") 
                    " | Speed: " (:speed @state) "x"))
      (println "Controls: [SPACE]=pause/play, [j]=next frame, [k]=previous frame, [.]=speed up, [,]=speed down, [q]=quit")
      
      ;; Display debug info in development mode
      (when (seq (:debug-log @state))
        (println "Last key:" (last (:debug-log @state))))
      
      (println (apply str (repeat 100 "-")))
      
      ;; Display header and data
      (display-header)
      (println (apply str (repeat 100 "-")))
      
      (doseq [row (take display-rows sorted-frame)]
        (println (format-ticker-row row)))
      
      (println (apply str (repeat 100 "-"))))))

;; Check if we're in an interactive terminal
(defn interactive-terminal? []
  (try
    (= 0 (:exit (shell/sh "bash" "-c" "tty -s")))
    (catch Exception _ false)))

;; Set up raw terminal mode for immediate key input
(defn setup-raw-terminal []
  (when (interactive-terminal?)
    (execute-bash "stty -echo -icanon min 1 time 0"))  ;; More explicit raw mode settings
  (add-debug-log "Terminal set to raw mode"))

;; Restore normal terminal mode
(defn restore-terminal []
  (when (interactive-terminal?)
    (execute-bash "stty echo icanon"))  ;; Simplified restore command
  (add-debug-log "Terminal restored to normal mode"))

;; Function to check for key presses - improved version
(defn check-key-press []
  (try
    (let [available (.available System/in)]
      (when (> available 0)
        (let [key-code (.read System/in)]
          (add-debug-log (str "Key pressed: " key-code " (char: " (char key-code) ")"))
          (case key-code
            32 :pause    ; space
            106 :next    ; j (next frame)
            107 :prev    ; k (previous frame)
            46 :faster   ; . (speed up)
            44 :slower   ; , (speed down)
            113 :quit    ; q (quit)
            nil))))
    (catch Exception e
      (add-debug-log (str "Error checking key press: " (.getMessage e)))
      nil)))

;; Non-blocking sleep that checks for key presses
(defn non-blocking-sleep [ms]
  (try
    (let [end-time (+ (System/currentTimeMillis) ms)
          check-interval 10] ; Check more frequently (10ms) for better responsiveness
      (loop []
        (let [current-time (System/currentTimeMillis)
              key-press (check-key-press)]
          (cond
            ;; If next key is pressed
            (= key-press :next) 
            (do (next-frame) :next)
            
            ;; If prev key is pressed
            (= key-press :prev)
            (do (prev-frame) :prev)
            
            ;; If faster key is pressed
            (= key-press :faster)
            (do (speed-up) :faster)
            
            ;; If slower key is pressed
            (= key-press :slower)
            (do (speed-down) :slower)
            
            ;; If pause key is pressed
            (= key-press :pause)
            (do (toggle-pause) :pause)
            
            ;; If quit key is pressed
            (= key-press :quit)
            (do 
              (restore-terminal)
              (tui/clear-screen) 
              (println "Exiting ticker replay...") 
              (System/exit 0))
            
            ;; If we've waited long enough, return nil
            (>= current-time end-time) 
            nil
            
            ;; Otherwise, sleep a small amount and check again
            :else
            (do
              (Thread/sleep (long check-interval))
              (recur))))))
    (catch Exception e
      (add-debug-log (str "Sleep error: " (.getMessage e)))
      nil)))

;; Main replay loop for non-interactive mode
(defn non-interactive-replay-loop []
  (doseq [index (range (count (:files @state)))]
    (load-frames index)
    (when-let [frame (calculate-speed (:last-frame @state) (:current-frame @state))]
      (display-frame frame))
    (Thread/sleep (long (/ 3000 (:speed @state))))))

;; Main replay loop for interactive mode
(defn interactive-replay-loop []
  (try
    (while true
      ;; Display current frame
      (when-let [frame (calculate-speed (:last-frame @state) (:current-frame @state))]
        (display-frame frame))
      
      ;; Check for key presses first
      (let [key-press (check-key-press)]
        (when key-press
          (add-debug-log (str "Processed key: " key-press))))
      
      ;; Check if it's time to advance to the next frame
      (let [current-time (System/currentTimeMillis)
            elapsed-time (- current-time (:last-update-time @state))
            frame-delay (/ 3000 (:speed @state))]
        
        ;; If not paused and enough time has passed, advance to next frame
        (if (and (not (:paused @state))
                 (>= elapsed-time frame-delay)
                 (< (:current-index @state) (dec (count (:files @state)))))
          (next-frame)
          
          ;; Otherwise, wait a bit and check for input
          (non-blocking-sleep 50))))
    (catch Exception e
      (restore-terminal)
      (println "Error in replay loop:" (.getMessage e))
      (println "Debug log:" (str/join "\n" (:debug-log @state))))))

;; Main entry point
(defn -main [& args]
  (let [date-str (or (first args) "2025-06-27")]
    (println "Loading ticker file list from" date-str)
    (let [file-count (initialize-state date-str)]
      (println "Found" file-count "ticker frames")
      
      ;; Load only the first frame to start
      (load-frames 0)
      
      (println "Starting replay in 3 seconds...")
      (println "Controls: [SPACE]=pause/play, [j]=next frame, [k]=previous frame, [.]=speed up, [,]=speed down, [q]=quit")
      (Thread/sleep (long 300))
      
      ;; Set up raw terminal mode
      (setup-raw-terminal)
      
      (try
        (interactive-replay-loop)
        (catch Exception e
          (restore-terminal)
          (println "Error:" (.getMessage e))
          (println "Debug log:" (str/join "\n" (:debug-log @state))))
        (finally
          (restore-terminal))))))
