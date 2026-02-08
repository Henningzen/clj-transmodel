;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/generator/timetable.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2026
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.generator.timetable
  "Generate flat timetable from NeTEx data for SQL export"
  (:require [jansenh.transmodel.netex.calendar :as cal]
            [clojure.string :as str])
  (:import [java.time LocalDate LocalTime LocalDateTime DayOfWeek]
           [java.time.format DateTimeFormatter]))

;; =============================================================================
;; ServiceJourney Parsing
;; =============================================================================

(def netex-ns "xmlns.http%3A%2F%2Fwww.netex.org.uk%2Fnetex")

(defn nkw [s] (keyword netex-ns s))

(defn find-child [elem tag]
  (->> (:content elem)
       (filter #(or (= (:tag %) tag)
                    (= (some-> (:tag %) name) (name tag))))
       first))

(defn find-children [elem tag]
  (->> (:content elem)
       (filter #(or (= (:tag %) tag)
                    (= (some-> (:tag %) name) (name tag))))))

(defn text-content [elem]
  (when elem
    (->> (:content elem)
         (filter string?)
         (apply str)
         str/trim)))

(defn attr [elem k]
  (get (:attrs elem) k))

(defn parse-time
  "Parse HH:mm:ss to LocalTime"
  [time-str]
  (when (and time-str (not (str/blank? time-str)))
    (try
      (LocalTime/parse (str/trim time-str))
      (catch Exception _ nil))))

(defn parse-passing-time
  "Parse TimetabledPassingTime element"
  [elem]
  (let [stop-ref-elem (find-child elem "StopPointInJourneyPatternRef")
        dep-elem (find-child elem "DepartureTime")
        arr-elem (find-child elem "ArrivalTime")
        dep-offset-elem (find-child elem "DepartureDayOffset")
        arr-offset-elem (find-child elem "ArrivalDayOffset")]
    {:stop-point-ref (when stop-ref-elem (attr stop-ref-elem :ref))
     :departure-time (parse-time (text-content dep-elem))
     :arrival-time (parse-time (text-content arr-elem))
     :departure-day-offset (some-> (text-content dep-offset-elem) parse-long)
     :arrival-day-offset (some-> (text-content arr-offset-elem) parse-long)}))

(defn parse-service-journey
  "Parse ServiceJourney element to map"
  [elem]
  (let [day-types-container (find-child elem "dayTypes")
        day-type-refs (->> (find-children day-types-container "DayTypeRef")
                           (map #(attr % :ref))
                           (remove nil?)
                           set)
        
        passing-times-container (find-child elem "passingTimes")
        passing-times (->> (find-children passing-times-container "TimetabledPassingTime")
                           (map parse-passing-time)
                           (sort-by #(or (:departure-time %) (:arrival-time %))))
        
        journey-pattern-ref-elem (find-child elem "JourneyPatternRef")
        operator-ref-elem (find-child elem "OperatorRef")
        line-ref-elem (find-child elem "LineRef")]
    
    {:id (attr elem :id)
     :version (attr elem :version)
     :name (text-content (find-child elem "Name"))
     :private-code (text-content (find-child elem "PrivateCode"))
     :day-type-refs day-type-refs
     :journey-pattern-ref (when journey-pattern-ref-elem 
                            (attr journey-pattern-ref-elem :ref))
     :operator-ref (when operator-ref-elem (attr operator-ref-elem :ref))
     :line-ref (when line-ref-elem (attr line-ref-elem :ref))
     :passing-times (vec passing-times)
     :first-departure (-> passing-times first :departure-time)
     :last-arrival (or (-> passing-times last :arrival-time)
                       (-> passing-times last :departure-time))
     :stop-count (count passing-times)}))

(defn collect-service-journeys
  "Walk XML and collect all ServiceJourney elements"
  [xml-root]
  (let [journeys (atom [])]
    (letfn [(walk [elem]
              (when (map? elem)
                (when (or (= (:tag elem) (nkw "ServiceJourney"))
                          (= (some-> (:tag elem) name) "ServiceJourney"))
                  (swap! journeys conj (parse-service-journey elem)))
                (doseq [child (:content elem)]
                  (walk child))))]
      (walk xml-root))
    @journeys))

;; =============================================================================
;; Timetable Generation
;; =============================================================================

(defn generate-trip-instance
  "Generate a single trip row for a specific date"
  [^LocalDate date service-journey]
  (let [first-stop (first (:passing-times service-journey))
        last-stop (last (:passing-times service-journey))
        dep-time (:departure-time first-stop)
        arr-time (or (:arrival-time last-stop) (:departure-time last-stop))
        
        ;; Handle day offset for overnight journeys
        arr-day-offset (or (:arrival-day-offset last-stop) 0)
        effective-arr-date (if (pos? arr-day-offset)
                             (.plusDays date arr-day-offset)
                             date)]
    
    {:trip-date date
     :day-of-week (.getDayOfWeek date)
     :departure-time dep-time
     :arrival-time arr-time
     :arrival-date effective-arr-date
     
     ;; For sorting
     :departure-datetime (when dep-time (LocalDateTime/of date dep-time))
     
     ;; Journey identification
     :journey-id (:id service-journey)
     :private-code (:private-code service-journey)
     :journey-name (:name service-journey)
     
     ;; References
     :line-ref (:line-ref service-journey)
     :operator-ref (:operator-ref service-journey)
     :journey-pattern-ref (:journey-pattern-ref service-journey)
     
     ;; Stops
     :from-stop-ref (:stop-point-ref first-stop)
     :to-stop-ref (:stop-point-ref last-stop)
     :stop-count (:stop-count service-journey)
     
     ;; Original day type (useful for debugging)
     :day-type-refs (:day-type-refs service-journey)}))

(defn generate-timetable
  "Generate complete timetable for a date range.
   
   Parameters:
   - calendar-index: from (cal/build-calendar-index shared-data)
   - service-journeys: from (collect-service-journeys line-data)
   - from-date: start of timetable period
   - to-date: end of timetable period
   
   Returns: sorted vector of trip rows"
  [calendar-index service-journeys ^LocalDate from-date ^LocalDate to-date]
  
  ;; Pre-compute operating dates for all day types
  (let [day-type-dates-cache
        (reduce
         (fn [cache dt-id]
           (assoc cache dt-id 
                  (set (cal/get-operating-dates calendar-index dt-id from-date to-date))))
         {}
         (distinct (mapcat :day-type-refs service-journeys)))]
    
    (->> service-journeys
         ;; For each journey, generate instances for all operating dates
         (mapcat
          (fn [sj]
            (let [;; Collect all dates from all day-type refs
                  operating-dates (->> (:day-type-refs sj)
                                       (mapcat #(get day-type-dates-cache %))
                                       set)]
              (for [date operating-dates]
                (generate-trip-instance date sj)))))
         
         ;; Sort by date, then departure time
         (sort-by (juxt :trip-date :departure-time))
         vec)))

;; =============================================================================
;; Output Formatting
;; =============================================================================

(def date-fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd"))
(def time-fmt (DateTimeFormatter/ofPattern "HH:mm"))
(def day-names {DayOfWeek/MONDAY "Mon" DayOfWeek/TUESDAY "Tue"
                DayOfWeek/WEDNESDAY "Wed" DayOfWeek/THURSDAY "Thu"
                DayOfWeek/FRIDAY "Fri" DayOfWeek/SATURDAY "Sat"
                DayOfWeek/SUNDAY "Sun"})

(defn format-trip-for-display
  "Format a trip row for human reading"
  [trip]
  {:date (.format ^LocalDate (:trip-date trip) date-fmt)
   :day (get day-names (:day-of-week trip))
   :departure (some-> (:departure-time trip) (.format time-fmt))
   :arrival (some-> (:arrival-time trip) (.format time-fmt))
   :journey-name (:journey-name trip)
   :private-code (:private-code trip)
   :line (:line-ref trip)
   :stops (:stop-count trip)})

(defn trip->sql-row
  "Convert trip to SQL INSERT-ready map"
  [trip]
  {:trip_date (:trip-date trip)
   :day_of_week (some-> (:day-of-week trip) .name)
   :departure_time (:departure-time trip)
   :arrival_time (:arrival-time trip)
   :journey_id (:journey-id trip)
   :private_code (:private-code trip)
   :journey_name (:journey-name trip)
   :line_ref (:line-ref trip)
   :operator_ref (:operator-ref trip)
   :from_stop_ref (:from-stop-ref trip)
   :to_stop_ref (:to-stop-ref trip)
   :stop_count (:stop-count trip)})



;; =============================================================================
;; SQL Generation
;; =============================================================================

(def create-table-ddl
  "CREATE TABLE IF NOT EXISTS scheduled_trips (
    id SERIAL PRIMARY KEY,
    trip_date DATE NOT NULL,
    day_of_week VARCHAR(10),
    departure_time TIME,
    arrival_time TIME,
    journey_id VARCHAR(255) NOT NULL,
    private_code VARCHAR(50),
    journey_name VARCHAR(255),
    line_ref VARCHAR(255),
    operator_ref VARCHAR(255),
    from_stop_ref VARCHAR(255),
    to_stop_ref VARCHAR(255),
    stop_count INTEGER,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );

  CREATE INDEX IF NOT EXISTS idx_trip_date ON scheduled_trips(trip_date);
  CREATE INDEX IF NOT EXISTS idx_departure ON scheduled_trips(trip_date, departure_time);
  CREATE INDEX IF NOT EXISTS idx_line ON scheduled_trips(line_ref);")

(defn generate-insert-sql
  "Generate SQL INSERT statement for a trip"
  [trip]
  (let [row (trip->sql-row trip)]
    (format "INSERT INTO scheduled_trips (trip_date, day_of_week, departure_time, arrival_time, journey_id, private_code, journey_name, line_ref, operator_ref, from_stop_ref, to_stop_ref, stop_count) VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', %d);"
            (:trip_date row)
            (:day_of_week row)
            (:departure_time row)
            (:arrival_time row)
            (:journey_id row)
            (or (:private_code row) "")
            (or (:journey_name row) "")
            (or (:line_ref row) "")
            (or (:operator_ref row) "")
            (or (:from_stop_ref row) "")
            (or (:to_stop_ref row) "")
            (or (:stop_count row) 0))))



(defn timetable-for-date
  "Generate timetable for a single specific date.
   
   Parameters:
   - calendar-index: from (cal/build-calendar-index shared-data)
   - service-journeys: from (collect-service-journeys line-data)
   - date: LocalDate or string 'yyyy-MM-dd'
   
   Returns: sorted vector of trips for that day"
  [calendar-index service-journeys date]
  (let [^LocalDate local-date (if (string? date)
                                (LocalDate/parse date)
                                date)]
    (generate-timetable calendar-index service-journeys local-date local-date)))

(defn today
  "Generate timetable for today"
  [calendar-index service-journeys]
  (timetable-for-date calendar-index service-journeys (LocalDate/now)))

(defn tomorrow
  "Generate timetable for tomorrow"
  [calendar-index service-journeys]
  (timetable-for-date calendar-index service-journeys (.plusDays (LocalDate/now) 1)))


;; =============================================================================
;;
;;   Side effects - non-pure functions
;;
;;   (TODO: migrate to a Utils folder
;;
;; =============================================================================

(defn print-timetable
  "Print timetable in human-readable format"
  [timetable & {:keys [limit] :or {limit 50}}]
  (let [trips (take limit timetable)
        formatted (map format-trip-for-display trips)]
    (println (format "%-12s %-4s %-6s %-6s %-20s %-8s %-5s"
                     "Date" "Day" "Dep" "Arr" "Name" "Code" "Stops"))
    (println (apply str (repeat 70 "-")))
    (doseq [t formatted]
      (println (format "%-12s %-4s %-6s %-6s %-20s %-8s %-5s"
                       (:date t)
                       (:day t)
                       (or (:departure t) "-")
                       (or (:arrival t) "-")
                       (or (:journey-name t) "-")
                       (or (:private-code t) "-")
                       (or (:stops t) "-"))))
    (when (> (count timetable) limit)
      (println (format "... and %d more trips" (- (count timetable) limit))))))

(defn print-daily-timetable
  "Print a day's timetable in traffic coordinator format"
  [calendar-index service-journeys date]
  (let [trips (timetable-for-date calendar-index service-journeys date)
        ^LocalDate local-date (if (string? date) (LocalDate/parse date) date)]
    (println)
    (println (format "=== %s (%s) ===" 
                     (.format local-date date-fmt)
                     (get day-names (.getDayOfWeek local-date))))
    (println (format "Line: %s | Trips: %d" 
                     (-> trips first :line-ref)
                     (count trips)))
    (println)
    (println (format "%-6s %-6s %-20s %-8s" "Dep" "Arr" "Destination" "Code"))
    (println (apply str (repeat 45 "-")))
    (doseq [trip trips]
      (println (format "%-6s %-6s %-20s %-8s"
                       (some-> (:departure-time trip) (.format time-fmt))
                       (some-> (:arrival-time trip) (.format time-fmt))
                       (or (:journey-name trip) "-")
                       (or (:private-code trip) "-"))))
    (println)))
