(ns knock.workers
  (:require
   [clojure.core.async :as async :refer [go go-loop chan >! <! >!! <!! close! alt! timeout]]
   [knock.utils :refer [cur-time-str println! ->uuid! pause pp-hashmap!]]))

;; Worker pool state
(def worker-pools (atom {}))

(defn create-worker-pool
  "Create a worker pool with specified number of workers and function to execute"
  [pool-id worker-count worker-fn & {:keys [queue-size]
                                     :or {queue-size 1000}}]
  (let [job-queue (chan queue-size)
        result-chan (chan 1000)
        workers (atom {})
        stats (atom {:total-processed 0
                     :total-queued 0
                     :queue-size 0
                     :active-workers 0})]
    
    ;; Create workers
    (doseq [worker-id (range worker-count)]
      (let [worker-state (atom {:id worker-id
                               :status :idle
                               :current-job nil
                               :processed-count 0
                               :start-time nil})]
        (swap! workers assoc worker-id worker-state)
        
        ;; Start worker go-loop
        (go-loop []
          (when-some [job (<! job-queue)]
            (let [{:keys [job-id args result-chan]} job]
              ;; Update worker state to working
              (swap! worker-state assoc 
                     :status :working
                     :current-job job-id
                     :start-time (System/currentTimeMillis))
              (swap! stats update :active-workers inc)
              
              (try
                ;; Execute the job
                (let [result (apply worker-fn args)]
                  ;; Send result back
                  (>! result-chan {:job-id job-id
                                  :result result
                                  :status :success
                                  :worker-id worker-id})
                  
                  ;; Update worker stats
                  (swap! worker-state update :processed-count inc)
                  (swap! stats update :total-processed inc))
                
                (catch Exception e
                  ;; Send error result
                  (>! result-chan {:job-id job-id
                                  :error (.getMessage e)
                                  :status :error
                                  :worker-id worker-id})))
              
              ;; Update worker state back to idle
              (swap! worker-state assoc 
                     :status :idle
                     :current-job nil
                     :start-time nil)
              (swap! stats update :active-workers dec)
              
              (recur))))))
    
    ;; Create pool interface
    (let [pool {:id pool-id
                :job-queue job-queue
                :result-chan result-chan
                :workers workers
                :stats stats
                :worker-fn worker-fn
                :created-at (cur-time-str)}]
      
      ;; Store in global registry
      (swap! worker-pools assoc pool-id pool)
      
      ;; Return pool interface
      pool)))

(defn submit-job
  "Submit a job to the worker pool"
  [pool & args]
  (let [job-id (->uuid! (str (:id pool) "-" (cur-time-str) "-" (rand-int 10000)))
        job {:job-id job-id
             :args args
             :result-chan (:result-chan pool)
             :submitted-at (cur-time-str)}]
    
    ;; Update queue stats
    (swap! (:stats pool) update :total-queued inc)
    (swap! (:stats pool) update :queue-size inc)
    
    ;; Submit job (non-blocking)
    (go
      (>! (:job-queue pool) job)
      (swap! (:stats pool) update :queue-size dec))
    
    ;; Return job ID for tracking
    job-id))

(defn get-result
  "Get result for a specific job (blocking)"
  [pool job-id & {:keys [timeout-ms]
                  :or {timeout-ms 30000}}]
  (go-loop [timeout-chan (timeout timeout-ms)]
    (alt!
      (:result-chan pool) ([result]
                          (if (= (:job-id result) job-id)
                            result
                            (recur (timeout timeout-ms))))
      timeout-chan {:job-id job-id
                   :status :timeout
                   :error "Job timed out"})))

(defn get-next-result
  "Get the next available result (blocking)"
  [pool & {:keys [timeout-ms]
           :or {timeout-ms 5000}}]
  (<!! (go
         (alt!
           (:result-chan pool) ([result] result)
           (timeout timeout-ms) {:status :timeout}))))

(defn pool-status
  "Get current status of the worker pool"
  [pool]
  (let [workers @(:workers pool)
        stats @(:stats pool)
        current-time (System/currentTimeMillis)
        worker-details (map (fn [[id state]]
                             (let [s @state
                                   working-seconds (if (and (:start-time s) (= (:status s) :working))
                                                    (quot (- current-time (:start-time s)) 1000)
                                                    "")]
                               {:worker-id id
                                :status (:status s)
                                :processed-count (:processed-count s)
                                :current-job (or (:current-job s) "")
                                :working-seconds working-seconds}))
                           workers)]
    
    {:pool-id (:id pool)
     :created-at (:created-at pool)
     :stats stats
     :workers worker-details}))

(defn print-pool-status
  "Print formatted pool status in column format"
  [pool]
  (let [status (pool-status pool)]
    (println! (str "=== Worker Pool: " (:pool-id status) " ==="))
    (println! (str "Created: " (:created-at status)))
    (println! (str "Stats: " (:stats status)))
    (println! "")
    (println! "Workers:")
    ;; Use pp-hashmap! for column format display with correct order
    (pp-hashmap! (:workers status) :worker-id :status :processed-count :current-job :working-seconds)
    (println! "")))

(defn shutdown-pool
  "Shutdown a worker pool"
  [pool]
  (close! (:job-queue pool))
  (close! (:result-chan pool))
  (swap! worker-pools dissoc (:id pool))
  (println! (str "Pool " (:id pool) " shutdown")))

(defn list-pools
  "List all active worker pools"
  []
  (keys @worker-pools))

(defn get-pool
  "Get pool by ID"
  [pool-id]
  (get @worker-pools pool-id))

;; Monitoring functions
(defn start-pool-monitor
  "Start monitoring a pool with periodic status updates"
  [pool & {:keys [interval-ms]
           :or {interval-ms 2000}}]
  (go-loop []
    (print-pool-status pool)
    (<! (timeout interval-ms))
    (when (get @worker-pools (:id pool))
      (recur))))

;; Convenience functions
(defn workers
  "Create and return a worker pool interface"
  [pool-id worker-count worker-fn & opts]
  (apply create-worker-pool pool-id worker-count worker-fn opts))

(defn submit!
  "Submit job to pool (alias for submit-job)"
  [pool & args]
  (apply submit-job pool args))

(defn status!
  "Print pool status (alias for print-pool-status)"
  [pool]
  (print-pool-status pool))

(defn result!
  "Get next result (alias for get-next-result)"
  [pool & opts]
  (apply get-next-result pool opts))

(defn monitor!
  "Start monitoring pool (alias for start-pool-monitor)"
  [pool & opts]
  (apply start-pool-monitor pool opts))

;; Example usage and demo
(defn demo-worker-fn
  "Demo function that simulates work"
  [task-name duration-ms]
  (println! (str "Starting task: " task-name))
  (pause duration-ms)
  (println! (str "Completed task: " task-name))
  {:task task-name :duration duration-ms :completed-at (cur-time-str)})

(defn demo
  "Demonstrate the worker pool system"
  []
  (println! "=== Worker Pool Demo ===")
  
  ;; Create a pool with 3 workers
  (let [pool (workers "demo-pool" 3 demo-worker-fn)]
    
    ;; Start monitoring
    (monitor! pool)
    
    ;; Submit some jobs
    (println! "Submitting jobs...")
    (doseq [i (range 10)]
      (submit! pool (str "task-" i) (+ 1000 (rand-int 3000))))
    
    ;; Get results as they complete
    (println! "Waiting for results...")
    (dotimes [_ 10]
      (let [result (result! pool :timeout-ms 10000)]
        (if (= (:status result) :timeout)
          (println! "Timeout waiting for result")
          (println! (str "Got result: " result)))))
    
    ;; Final status
    (status! pool)
    
    ;; Shutdown
    (pause 2000)
    (shutdown-pool pool)))

(defn simple-demo
  "Simple demonstration of the worker pool system"
  []
  (println "=== Simple Worker Pool Demo ===")
  
  ;; Create a pool with 2 workers
  (let [pool (workers "simple-pool" 2 (fn [x] (* x x)))]
    
    (println "Created pool with 2 workers")
    (status! pool)
    
    ;; Submit some jobs
    (println "Submitting 5 jobs...")
    (doseq [i (range 1 6)]
      (let [job-id (submit! pool i)]
        (println (str "Submitted job " job-id " with input " i))))
    
    ;; Wait a bit for processing
    (Thread/sleep 1000)
    
    ;; Get results
    (println "Getting results...")
    (dotimes [_ 5]
      (let [result (result! pool :timeout-ms 5000)]
        (if (= (:status result) :timeout)
          (println "Timeout waiting for result")
          (println (str "Got result: " (:result result) " from job " (:job-id result))))))
    
    ;; Final status
    (println "Final status:")
    (status! pool)
    
    ;; Shutdown
    (shutdown-pool pool)
    (println "Demo completed")))

(defn enhanced-demo
  "Enhanced demonstration with better monitoring"
  []
  (println "=== Enhanced Worker Pool Demo ===")
  (println "This demo will show workers processing jobs with real-time monitoring")
  (println)
  
  ;; Create a pool with 3 workers
  (let [pool (workers "enhanced-pool" 3 demo-worker-fn)]
    
    (println "Created pool with 3 workers")
    (status! pool)
    
    ;; Start monitoring in background
    (println "Starting background monitoring...")
    (monitor! pool :interval-ms 3000)
    
    ;; Submit jobs in batches
    (println "Submitting first batch of 5 jobs...")
    (doseq [i (range 1 6)]
      (submit! pool (str "batch1-task-" i) (+ 2000 (rand-int 3000)))
      (Thread/sleep 500)) ; Stagger submissions
    
    (Thread/sleep 2000)
    
    (println "Submitting second batch of 5 jobs...")
    (doseq [i (range 1 6)]
      (submit! pool (str "batch2-task-" i) (+ 1000 (rand-int 2000)))
      (Thread/sleep 300))
    
    (Thread/sleep 1000)
    
    (println "Submitting final batch of 3 jobs...")
    (doseq [i (range 1 4)]
      (submit! pool (str "final-task-" i) (+ 1500 (rand-int 2500)))
      (Thread/sleep 200))
    
    ;; Collect results
    (println)
    (println "Collecting results as they complete...")
    (dotimes [_ 13] ; Total jobs submitted
      (let [result (result! pool :timeout-ms 15000)]
        (if (= (:status result) :timeout)
          (println "⏰ Timeout waiting for result")
          (println (str "✅ Completed: " (:task (:result result)) 
                       " (took " (:duration (:result result)) "ms)"
                       " by worker-" (:worker-id result))))))
    
    (println)
    (println "All jobs completed! Final status:")
    (status! pool)
    
    ;; Keep monitoring for a bit to show final state
    (println "Monitoring final state for 5 seconds...")
    (Thread/sleep 5000)
    
    ;; Shutdown
    (shutdown-pool pool)
    (println "Enhanced demo completed!")))

(defn -main [& args]
  (case (first args)
    "demo" (demo)
    "simple" (simple-demo)
    "enhanced" (enhanced-demo)
    (do 
      (println "Usage:")
      (println "  bb workers-demo simple    - Run simple demo")
      (println "  bb workers-demo demo      - Run original full demo")
      (println "  bb workers-demo enhanced  - Run enhanced demo with monitoring"))))

(comment
  ;; Usage examples:
  
  ;; 1. Create a worker pool
  (def my-pool (workers "my-pool" 4 #(* % %)))
  
  ;; 2. Submit jobs
  (submit! my-pool 5)
  (submit! my-pool 10)
  (submit! my-pool 15)
  
  ;; 3. Get results
  (result! my-pool)
  
  ;; 4. Check status
  (status! my-pool)
  
  ;; 5. Start monitoring
  (monitor! my-pool)
  
  ;; 6. Run demo
  (demo))
