(ns knock.asciinema-player
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [babashka.process :as process]))

;; Define the path for storing the speed setting
(def speed-file "/tmp/asciinema-player-speed.txt")

;; Function to update the playback speed
(defn update-speed [current-speed key-press]
  (let [new-speed (case key-press
                    :faster (min 8.0 (* current-speed 2.0))
                    :slower (max 0.25 (/ current-speed 2.0))
                    current-speed)]
    ;; Store the updated speed in file
    (when (not= current-speed new-speed)
      (spit speed-file (str new-speed)))
    new-speed))

;; Function to get the current playback speed
(defn get-current-speed [default-speed]
  (try
    (if (.exists (io/file speed-file))
      (Double/parseDouble (str/trim (slurp speed-file)))
      default-speed)
    (catch Exception e
      default-speed)))

;; Function to check for key presses
(defn check-key-press []
  (try
    (let [available (.available System/in)]
      (when (> available 0)
        (let [key-code (.read System/in)]
          (case key-code
            27 :quit     ; ESC
            32 :pause    ; space
            46 :faster   ; .
            44 :slower   ; ,
            39 :right    ; '
            37 :left     ; %
            106 :next    ; j (next frame)
            113 :quit    ; q (quit)
            nil))))
    (catch Exception e
      nil)))

;; Function for non-blocking sleep that checks for key presses
(defn non-blocking-sleep [milliseconds]
  (let [end-time (+ (System/currentTimeMillis) milliseconds)
        check-interval 5] ; Check every 5ms for better responsiveness
    (loop []
      (let [current-time (System/currentTimeMillis)
            key-press (check-key-press)]
        (cond
          ;; If j key is pressed, return :next
          (= key-press :next) 
          :next
          
          ;; If faster key is pressed, return :faster
          (= key-press :faster)
          :faster
          
          ;; If slower key is pressed, return :slower
          (= key-press :slower)
          :slower
          
          ;; If quit key is pressed, return :quit
          (= key-press :quit)
          :quit
          
          ;; If pause key is pressed, return :pause
          (= key-press :pause)
          :pause
          
          ;; If we've waited long enough, return nil
          (>= current-time end-time) 
          nil
          
          ;; Otherwise, sleep a small amount and check again
          :else
          (do
            (Thread/sleep check-interval)
            (recur)))))))

;; Function to set up raw terminal mode using stty
(defn setup-raw-mode []
  (try
    ;; Use babashka.process to run stty commands
    (let [process (process/process ["stty", "raw", "-echo"] {:inherit true})]
      (let [exit-code (-> process deref :exit)]
        (if (zero? exit-code)
          (println "Terminal set to raw mode")
          (println "Failed to set terminal to raw mode"))))
    true
    (catch Exception e
      (println "Note: Could not set terminal to raw mode -" (.getMessage e))
      false)))

;; Function to restore terminal settings
(defn restore-terminal []
  (try
    ;; Use babashka.process to restore terminal settings
    (let [process (process/process ["stty", "sane"] {:inherit true})]
      (let [exit-code (-> process deref :exit)]
        (if (zero? exit-code)
          (println "Terminal settings restored")
          (println "Failed to restore terminal settings"))))
    (catch Exception e
      (println "Note: Could not restore terminal settings -" (.getMessage e)))))

;; Function to get file creation time in milliseconds
(defn get-file-creation-time [file-path]
  (try
    (let [file (io/file file-path)
          attrs (java.nio.file.Files/readAttributes 
                 (.toPath file)
                 java.nio.file.attribute.BasicFileAttributes
                 (make-array java.nio.file.LinkOption 0))]
      (long (.toMillis (.creationTime attrs))))
    (catch Exception e
      ;; Fallback to last modified time if creation time is not available
      (long (.lastModified (io/file file-path))))))

;; Function to format time as yyyy-MM-dd HH:MM:SS
(defn format-time [timestamp]
  (let [date (java.util.Date. (long timestamp))
        formatter (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss")]
    (.format formatter date)))

;; Function to draw the progress bar at the top of the screen
(defn draw-progress-bar [progress speed event-count total-events total-duration current-time paused? file-creation-time]
  (let [bar-width 25  ; Further reduced width to make room for ISO date format
        filled-width (int (* bar-width progress))
        empty-width (- bar-width filled-width)
        filled-bar (apply str (repeat filled-width "="))
        empty-bar (apply str (repeat empty-width " "))
        percentage (int (* 100 progress))
        ;; Calculate current real time: file creation time + playback offset
        current-real-time (+ file-creation-time (* current-time 1000))
        time-display (format-time current-real-time)
        shortcuts " [ESC/q:quit j:next-line SPACE:pause .:faster ,:slower]"
        status-line (format " %.2f [%s%s] %3d%% %.1f/%.1f | %s %s%s"
                            (double speed)
                            filled-bar
                            empty-bar
                            percentage
                            (double current-time)
                            (double total-duration)
                            time-display
                            (if paused? "(paused)" "")
                            shortcuts)]
    ;; Save current cursor position
    (print "\u001b[s")
    ;; Move cursor to the top of the screen
    (print "\u001b[1;1H")
    ;; Clear the line
    (print "\u001b[K")
    ;; Print colored progress bar
    (print (str "\u001b[1;37;44m" status-line " \u001b[0m"))
    ;; Restore cursor position
    (print "\u001b[u")
    (flush)))

;; Function for non-blocking sleep that checks for key presses and updates progress
(defn non-blocking-sleep-with-progress [milliseconds current-time total-duration speed event-count total-events file-creation-time]
  (let [end-time (+ (System/currentTimeMillis) milliseconds)
        check-interval 50 ; Check every 50ms and update progress
        start-real-time (System/currentTimeMillis)]
    (loop [last-progress-update 0]
      (let [current-real-time (System/currentTimeMillis)
            elapsed-real-time (- current-real-time start-real-time)
            simulated-time (+ current-time (/ elapsed-real-time 1000.0))
            key-press (check-key-press)]
        (cond
          ;; If j key is pressed, return :next
          (= key-press :next) 
          :next
          
          ;; If faster key is pressed, return :faster
          (= key-press :faster)
          :faster
          
          ;; If slower key is pressed, return :slower
          (= key-press :slower)
          :slower
          
          ;; If quit key is pressed, return :quit
          (= key-press :quit)
          :quit
          
          ;; If pause key is pressed, return :pause
          (= key-press :pause)
          :pause
          
          ;; If we've waited long enough, return nil
          (>= current-real-time end-time) 
          nil
          
          ;; Otherwise, update progress if enough time has passed and continue
          :else
          (do
            ;; Update progress bar every 200ms during long pauses
            (when (> (- current-real-time last-progress-update) 200)
              (let [progress-pct (/ simulated-time total-duration)]
                (draw-progress-bar progress-pct speed event-count total-events total-duration simulated-time false file-creation-time)))
            
            (Thread/sleep check-interval)
            (recur (if (> (- current-real-time last-progress-update) 200)
                     current-real-time
                     last-progress-update))))))))

;; Base64 decode function
(defn base64-decode [s]
  (String. (.decode (java.util.Base64/getDecoder) (.getBytes s))))

;; Function to check if an event ends with a newline
(defn frame-end? [content]
  (when (string? content)
    (or (str/ends-with? content "\r\n")
        (str/ends-with? content "\n"))))

;; Function to find the next frame end event index
(defn find-next-frame-end [events current-index]
  (loop [idx (inc current-index)]
    (if (>= idx (count events))
      nil  ; No more frame ends found
      (let [[_ _ content] (nth events idx)]
        (if (frame-end? content)
          idx
          (recur (inc idx)))))))

;; Function to play events from start to end index
(defn play-events-range [events start-idx end-idx speed]
  (loop [idx start-idx
         last-time (if (> start-idx 0) 
                     (first (nth events (dec start-idx)))
                     0.0)]
    (if (< idx end-idx)
      (let [[time type content] (nth events idx)
            delay-time (/ (- time last-time) speed)]
        
        ;; Wait for the appropriate delay with non-blocking sleep
        (when (> delay-time 0)
          (let [sleep-result (non-blocking-sleep (long (* 1000 delay-time)))]
            ;; If user wants to quit during fast playback, respect it
            (when (= sleep-result :quit)
              (throw (ex-info "User quit" {:type :user-quit})))))
        
        ;; Print the content
        (if (and (= type "o") (string? content))
          (let [lines (str/split content #"\r?\n")]
            (doseq [[i line] (map-indexed vector lines)]
              (when (> i 0)
                ;; For lines after the first one, move cursor to beginning of line first
                (print "\r\n"))
              (print line))
            ;; Always print the trailing newline if this is a frame end
            (when (frame-end? content)
              (print "\r\n")))
          (print content))
        (flush)
        
        (recur (inc idx) time))
      nil)))

;; Main function to play an asciinema recording
(defn play-recording [file & {:keys [speed]
                              :or {speed 1.0}}]
  (println "Replaying" file "at speed" speed)
  (println "Controls:")
  (println "  Right Arrow or '.' - Speed up playback")
  (println "  Left Arrow or ',' - Slow down playback")
  (println "  j - Next frame (after newline)")
  (println "  Space - Pause/Resume")
  (println "  ESC or q - Quit")

  ;; Initialize speed from file cache or use default
  (let [initial-speed (get-current-speed speed)]

    ;; Read the recording file
    (let [content (slurp file)
          lines (str/split-lines content)
          header (first lines)
          events (rest lines)
          output-events (map (fn [line]
                               (try
                                 (let [parsed (read-string line)
                                       time (first parsed)
                                       type (second parsed)
                                       content (nth parsed 2 "")]
                                   [time
                                    type
                                    (if (= type "o")
                                      content
                                      content)])
                                 (catch Exception e
                                   (println "Error parsing line:" line)
                                   [0.0 "o" ""])))
                             events)
          total-events (count output-events)
          total-duration (if (empty? output-events)
                           0.0
                           (first (first (reverse output-events))))
          ;; Get file creation time for real-time display
          file-creation-time (get-file-creation-time file)]

      (println "Total duration:" (format "%.2f" total-duration) "seconds")
      (println "Total events:" total-events)
      (println "File created at:" (format-time file-creation-time))
      (println "Starting replay in 1 seconds...")
      (Thread/sleep 1000)

      ;; Clear screen and set up for display
      (print "\u001b[2J\u001b[H")
      (flush)
      
      ;; Draw initial progress bar
      (draw-progress-bar 0.0 initial-speed 0 total-events total-duration 0.0 false file-creation-time)
      
      ;; Set terminal to raw mode for key capture
      (setup-raw-mode)
      
      (try
        (loop [current-idx 0
               last-time 0
               event-count 0
               current-speed initial-speed
               paused? false]
          
          (if (>= current-idx (count output-events))
            ;; End of events
            nil
            
            (let [[time type content] (nth output-events current-idx)
                  delay-time (/ (- time last-time) current-speed)
                  progress-pct (/ time total-duration)
                  new-event-count (inc event-count)]
              
              ;; Update progress bar
              (draw-progress-bar progress-pct current-speed new-event-count total-events total-duration time paused? file-creation-time)
              
              (if paused?
                ;; If paused, wait for key presses with proper delay and update display
                (do
                  ;; Update progress bar to show paused state
                  (draw-progress-bar progress-pct current-speed new-event-count total-events total-duration time true file-creation-time)
                  
                  ;; Wait with blocking sleep to prevent CPU spinning
                  (Thread/sleep 100)
                  
                  ;; Check for key press
                  (let [key-press (check-key-press)]
                    (case key-press
                      :quit nil
                      :pause (do
                               ;; Update display to show unpaused state
                               (draw-progress-bar progress-pct current-speed new-event-count total-events total-duration time false file-creation-time)
                               (recur current-idx last-time event-count current-speed false))
                      :next (let [next-frame-idx (find-next-frame-end output-events current-idx)]
                              (if next-frame-idx
                                (recur (inc next-frame-idx) 
                                       (if (< (inc next-frame-idx) (count output-events))
                                         (first (nth output-events (inc next-frame-idx)))
                                         total-duration)
                                       (+ event-count (- (inc next-frame-idx) current-idx))
                                       current-speed
                                       false)
                                (recur current-idx last-time event-count current-speed false)))
                      :faster (recur current-idx last-time event-count (update-speed current-speed :faster) paused?)
                      :slower (recur current-idx last-time event-count (update-speed current-speed :slower) paused?)
                      ;; No key pressed, continue in paused state
                      (recur current-idx last-time event-count current-speed paused?))))
                
                ;; Not paused, normal playback
                (let [key-press (check-key-press)]
                  (case key-press
                    :quit nil
                    :pause (recur current-idx last-time event-count current-speed true)
                    :next (let [next-frame-idx (find-next-frame-end output-events current-idx)]
                            (if next-frame-idx
                              (do
                                ;; First, finish playing the current frame if we're in the middle of it
                                (when (< current-idx next-frame-idx)
                                  (try
                                    (play-events-range output-events current-idx (inc next-frame-idx) 8.0)
                                    (catch Exception e
                                      (when (= (:type (ex-data e)) :user-quit)
                                        (throw e)))))
                                
                                ;; Move to the next frame
                                (let [next-idx (inc next-frame-idx)
                                      next-time (if (< next-idx (count output-events))
                                                  (first (nth output-events next-idx))
                                                  total-duration)]
                                  
                                  ;; Draw progress bar
                                  (draw-progress-bar (/ next-time total-duration) current-speed (+ event-count (- next-idx current-idx)) total-events total-duration next-time paused? file-creation-time)
                                  
                                  ;; Continue from the next frame
                                  (recur next-idx next-time (+ event-count (- next-idx current-idx)) current-speed paused?)))
                              (recur current-idx last-time event-count current-speed paused?)))
                    :faster (recur current-idx last-time event-count (update-speed current-speed :faster) paused?)
                    :slower (recur current-idx last-time event-count (update-speed current-speed :slower) paused?)
                    nil (do
                          ;; Wait for the appropriate delay with non-blocking sleep
                          (if (> delay-time 0)
                            (let [sleep-result (if (> delay-time 1.0)
                                                 ;; For long delays (>1s), use progress-aware sleep
                                                 (non-blocking-sleep-with-progress (long (* 1000 delay-time)) time total-duration current-speed new-event-count total-events file-creation-time)
                                                 ;; For short delays, use regular non-blocking sleep
                                                 (non-blocking-sleep (long (* 1000 delay-time))))]
                              (case sleep-result
                                :next (let [next-frame-idx (find-next-frame-end output-events current-idx)]
                                        (if next-frame-idx
                                          (do
                                            ;; First, finish playing the current frame if we're in the middle of it
                                            (when (< current-idx next-frame-idx)
                                              (try
                                                (play-events-range output-events current-idx (inc next-frame-idx) 8.0)
                                                (catch Exception e
                                                  (when (= (:type (ex-data e)) :user-quit)
                                                    (throw e)))))
                                            
                                            ;; Move to the next frame
                                            (let [next-idx (inc next-frame-idx)
                                                  next-time (if (< next-idx (count output-events))
                                                              (first (nth output-events next-idx))
                                                              total-duration)]
                                              
                                              ;; Draw progress bar
                                              (draw-progress-bar (/ next-time total-duration) current-speed (+ event-count (- next-idx current-idx)) total-events total-duration next-time paused? file-creation-time)
                                              
                                              ;; Continue from the next frame
                                              (recur next-idx next-time (+ event-count (- next-idx current-idx)) current-speed paused?)))
                                          (recur (inc current-idx) time new-event-count current-speed paused?)))
                                :faster (recur current-idx last-time event-count (update-speed current-speed :faster) paused?)
                                :slower (recur current-idx last-time event-count (update-speed current-speed :slower) paused?)
                                :pause (recur current-idx last-time event-count current-speed true)
                                :quit nil
                                nil (do
                                      ;; Print the content
                                      (if (and (= type "o") (string? content))
                                        (let [lines (str/split content #"\r?\n")]
                                          (doseq [[i line] (map-indexed vector lines)]
                                            (when (> i 0)
                                              ;; For lines after the first one, move cursor to beginning of line first
                                              (print "\r\n"))
                                            (print line))
                                          ;; Always print the trailing newline if this is a frame end
                                          (when (frame-end? content)
                                            (print "\r\n")))
                                        (print content))
                                      (flush)
                                      
                                      ;; Process the next event
                                      (recur (inc current-idx) time new-event-count current-speed paused?))))
                            ;; No delay needed, just print the content
                            (do
                              ;; Print the content
                              (if (and (= type "o") (string? content))
                                (let [lines (str/split content #"\r?\n")]
                                  (doseq [[i line] (map-indexed vector lines)]
                                    (when (> i 0)
                                      ;; For lines after the first one, move cursor to beginning of line first
                                      (print "\r\n"))
                                    (print line))
                                  ;; Always print the trailing newline if this is a frame end
                                  (when (frame-end? content)
                                    (print "\r\n")))
                                (print content))
                              (flush)
                              
                              ;; Process the next event
                              (recur (inc current-idx) time new-event-count current-speed paused?))))))))))
        
        (catch Exception e
          ;; Handle user quit gracefully
          (when (not= (:type (ex-data e)) :user-quit)
            (println "\nError during playback:" (.getMessage e))))
        
        (finally
          ;; Restore terminal settings
          (restore-terminal)
          ;; Make sure to flush output
          (.flush System/out)
          (.flush System/err))))
    
    (println "\nReplay complete!")))

;; Function to convert asciinema recording to PowerPoint
(defn asciinema2ppt [input-file output-file]
  (println "Converting" input-file "to PowerPoint presentation" output-file)
  (let [result (try
                 (let [process (.exec (Runtime/getRuntime)
                                     (into-array String ["python3" "-m" "asciinema2ppt"
                                                        "--input" input-file
                                                        "--output" output-file]))]
                   (.waitFor process)
                   {:exit (.exitValue process)})
                 (catch Exception e
                   {:exit 1 :err (.getMessage e)}))]
    (if (= 0 (:exit result))
      (println "Conversion successful!")
      (println "Conversion failed:" (:err result)))))

;; Main entry point for the asciinema player
(defn -main [& args]
  (let [options (loop [args args
                       opts {}]
                  (if (empty? args)
                    opts
                    (let [arg (first args)]
                      (case arg
                        "--input" (recur (drop 2 args) (assoc opts :input (second args)))
                        "--speed" (recur (drop 2 args) (assoc opts :speed (Double/parseDouble (second args))))
                        (recur (rest args) opts)))))]
    (if-let [input-file (:input options)]
      (let [speed (or (:speed options) 1.0)]
        (play-recording input-file :speed speed))
      (println "Usage: bb -m knock.asciinema-player --input <recording-file> [--speed <speed>]"))))
