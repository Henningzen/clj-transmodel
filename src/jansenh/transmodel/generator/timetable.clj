;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/generator/timetable.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025 - 2026
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.generator.timetable
  "Generate flat and detailed timetables from NeTEx"
  (:require [jansenh.transmodel.netex.calendar :as cal]
            [jansenh.transmodel.netex.registry :as reg]
            [clojure.string :as str])
  (:import [java.time LocalDate LocalTime LocalDateTime DayOfWeek]
           [java.time.format DateTimeFormatter]))

;;
;;   Timetable generator
;;   -------------------
;;
;;   Supports data held in registry or raw xml datastructure
;;
;;   Uses the expanded calendar and the registry data for timetable generation.
;;   This namespace produces application specific generated Timetable.
;;
;;   authors:   Henning Jansen - henning.jansen@jansenh.no;
;;   since:     0.1.0   2025-08-11
;;   version:   0.2.1   2026-03-01
;;   ---------------------------------------------------------------------------
;;

;; =============================================================================
;; Time parsing (strings from extract → LocalTime)
;; =============================================================================

(defn- parse-time [time-str]
  (when (and time-str (not (str/blank? time-str)))
    (try (LocalTime/parse (str/trim time-str))
         (catch Exception _ nil))))

;; =============================================================================
;; Trip Generation
;; =============================================================================

(defn- enrich-passing-time
  "Add parsed LocalTime and resolved stop info to a passing-time map"
  [pt]
  (let [spijp-id (:stop-point-ref pt)
        stop-info (get (reg/spijp-index) spijp-id)]
    (assoc pt
           :departure-local-time (parse-time (:departure-time pt))
           :arrival-local-time (parse-time (:arrival-time pt))
           :scheduled-stop-ref (:scheduled-stop-ref stop-info)
           :stop-name (some-> (:scheduled-stop-ref stop-info) reg/stop-name)
           :for-boarding (get stop-info :for-boarding true)
           :for-alighting (get stop-info :for-alighting true)
           :destination-display-ref (:destination-display-ref stop-info))))

(defn- enrich-service-journey
  "Add derived fields to a service journey from the registry"
  [sj]
  (let [enriched-pts (mapv enrich-passing-time (:passing-times sj))
        first-pt (first enriched-pts)
        last-pt (last enriched-pts)]
    (assoc sj
           :passing-times enriched-pts
           :first-departure (:departure-local-time first-pt)
           :last-arrival (or (:arrival-local-time last-pt)
                             (:departure-local-time last-pt))
           :stop-count (count enriched-pts))))

(defn generate-trip-instance
  "Generate a single trip row for a specific date"
  [^LocalDate date sj]
  (let [first-pt (first (:passing-times sj))
        last-pt (last (:passing-times sj))
        arr-offset (or (:arrival-day-offset last-pt) 0)]
    {:trip-date date
     :day-of-week (.getDayOfWeek date)
     :departure-time (:first-departure sj)
     :arrival-time (:last-arrival sj)
     :arrival-date (if (pos? arr-offset) (.plusDays date arr-offset) date)
     :departure-datetime (when (:first-departure sj)
                           (LocalDateTime/of date (:first-departure sj)))
     :journey-id (:id sj)
     :private-code (:private-code sj)
     :journey-name (:name sj)
     :line-ref (:line-ref sj)
     :operator-ref (:operator-ref sj)
     :journey-pattern-ref (:journey-pattern-ref sj)
     :from-stop-ref (:scheduled-stop-ref first-pt)
     :from-stop-name (:stop-name first-pt)
     :to-stop-ref (:scheduled-stop-ref last-pt)
     :to-stop-name (:stop-name last-pt)
     :stop-count (:stop-count sj)
     :day-type-refs (:day-type-refs sj)}))

(defn generate-trip-stops
  "Generate stop-level detail for a trip instance"
  [^LocalDate date sj]
  (->> (:passing-times sj)
       (map-indexed
        (fn [idx pt]
          (let [dep-offset (or (:departure-day-offset pt) 0)
                arr-offset (or (:arrival-day-offset pt) 0)]
            {:sequence (inc idx)
             :stop-ref (:scheduled-stop-ref pt)
             :stop-name (:stop-name pt)
             :arrival-time (:arrival-local-time pt)
             :departure-time (:departure-local-time pt)
             :arrival-date (when (:arrival-local-time pt)
                             (.plusDays date arr-offset))
             :departure-date (when (:departure-local-time pt)
                               (.plusDays date dep-offset))
             :for-boarding (:for-boarding pt)
             :for-alighting (:for-alighting pt)
             :destination-display-ref (:destination-display-ref pt)
             ;; Parent context
             :trip-date date
             :journey-id (:id sj)
             :private-code (:private-code sj)})))
       vec))

;; =============================================================================
;; Timetable Generation (pure functions, registry-backed)
;; =============================================================================

(defn generate-timetable
  "Generate complete timetable for a date range.

   Parameters:
   - calendar-index: from (cal/build-calendar-index shared-data)
   - from-date / to-date: LocalDate range

   Reads service journeys from registry."
  [calendar-index ^LocalDate from-date ^LocalDate to-date]
  (let [service-journeys (->> (reg/all-service-journeys)
                              (map enrich-service-journey))

        day-type-dates-cache
        (reduce
         (fn [cache dt-id]
           (assoc cache dt-id
                  (set (cal/get-operating-dates
                        calendar-index dt-id from-date to-date))))
         {}
         (distinct (mapcat :day-type-refs service-journeys)))]

    (->> service-journeys
         (mapcat
          (fn [sj]
            (let [dates (->> (:day-type-refs sj)
                             (mapcat #(get day-type-dates-cache %))
                             set)]
              (for [date dates]
                (generate-trip-instance date sj)))))
         (sort-by (juxt :trip-date :departure-time))
         vec)))

(defn generate-detailed-timetable
  "Generate timetable with all stops expanded.

   Returns: vector of {:trip {...} :stops [{...} ...]} sorted by departure"
  [calendar-index ^LocalDate from-date ^LocalDate to-date]
  (let [service-journeys (->> (reg/all-service-journeys)
                              (map enrich-service-journey))

        sj-by-id (reduce (fn [m sj] (assoc m (:id sj) sj)) {} service-journeys)

        day-type-dates-cache
        (reduce
         (fn [cache dt-id]
           (assoc cache dt-id
                  (set (cal/get-operating-dates
                        calendar-index dt-id from-date to-date))))
         {}
         (distinct (mapcat :day-type-refs service-journeys)))]

    (->> service-journeys
         (mapcat
          (fn [sj]
            (let [dates (->> (:day-type-refs sj)
                             (mapcat #(get day-type-dates-cache %))
                             set)]
              (for [date dates]
                {:trip (generate-trip-instance date sj)
                 :stops (generate-trip-stops date sj)}))))
         (sort-by (comp (juxt :trip-date :departure-time) :trip))
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

(defn format-trip-for-display [trip]
  {:date (.format ^LocalDate (:trip-date trip) date-fmt)
   :day (get day-names (:day-of-week trip))
   :departure (some-> (:departure-time trip) (.format time-fmt))
   :arrival (some-> (:arrival-time trip) (.format time-fmt))
   :journey-name (:journey-name trip)
   :private-code (:private-code trip)
   :line (:line-ref trip)
   :stops (:stop-count trip)})

(defn print-timetable
  [timetable & {:keys [limit] :or {limit 50}}]
  (let [trips (take limit timetable)
        formatted (map format-trip-for-display trips)]
    (println (format "%-12s %-4s %-6s %-6s %-20s %-8s %-5s"
                     "Date" "Day" "Dep" "Arr" "Name" "Code" "Stops"))
    (println (apply str (repeat 70 "-")))
    (doseq [t formatted]
      (println (format "%-12s %-4s %-6s %-6s %-20s %-8s %-5s"
                       (:date t) (:day t)
                       (or (:departure t) "-")
                       (or (:arrival t) "-")
                       (or (:journey-name t) "-")
                       (or (:private-code t) "-")
                       (or (:stops t) "-"))))
    (when (> (count timetable) limit)
      (println (format "... and %d more trips" (- (count timetable) limit))))))

(defn print-trip-detail
  "Print a single trip with all its stops"
  [{:keys [trip stops]}]
  (let [date-str (.format ^LocalDate (:trip-date trip) date-fmt)
        day-str (get day-names (:day-of-week trip))]
    (println)
    (println (format "┌─ %s %s  %s → %s  [%s] %s"
                     date-str day-str
                     (some-> (:departure-time trip) (.format time-fmt))
                     (some-> (:arrival-time trip) (.format time-fmt))
                     (or (:private-code trip) "?")
                     (or (:journey-name trip) "")))
    (println "│")
    (doseq [stop stops]
      (let [arr (some-> (:arrival-time stop) (.format time-fmt))
            dep (some-> (:departure-time stop) (.format time-fmt))
            time-str (cond
                       (and arr dep) (format "%s/%s" arr dep)
                       dep           (format "  ··/%s" dep)
                       arr           (format "%s/··  " arr)
                       :else         "  ··/··  ")
            boarding (cond
                       (and (not (:for-boarding stop))
                            (not (:for-alighting stop))) " ✕"
                       (not (:for-boarding stop))         " ↓"
                       (not (:for-alighting stop))        " ↑"
                       :else                              "  ")
            overnight (if (and (:departure-date stop)
                               (not= (:departure-date stop) (:trip-date stop)))
                        " +1" "")]
        (println (format "│  %d. %-11s %s %-30s%s"
                         (:sequence stop)
                         time-str
                         boarding
                         (or (:stop-name stop) (:stop-ref stop) "?")
                         overnight))))
    (println "└──")))

(defn print-detailed-timetable
  [detailed-timetable & {:keys [limit] :or {limit 20}}]
  (let [trips (take limit detailed-timetable)]
    (doseq [t trips]
      (print-trip-detail t))
    (when (> (count detailed-timetable) limit)
      (println (format "\n... and %d more trips"
                       (- (count detailed-timetable) limit))))))

;; =============================================================================
;; Convenience
;; =============================================================================

(defn timetable-for-date
  [calendar-index date]
  (let [^LocalDate d (if (string? date) (LocalDate/parse date) date)]
    (generate-timetable calendar-index d d)))

(defn detailed-for-date
  [calendar-index date]
  (let [^LocalDate d (if (string? date) (LocalDate/parse date) date)]
    (generate-detailed-timetable calendar-index d d)))

(defn today [calendar-index]
  (timetable-for-date calendar-index (LocalDate/now)))

(defn print-daily-detailed
  [calendar-index date]
  (let [^LocalDate d (if (string? date) (LocalDate/parse date) date)
        detailed (detailed-for-date calendar-index d)]
    (println)
    (println (format "══════ %s (%s) ══════"
                     (.format d date-fmt)
                     (get day-names (.getDayOfWeek d))))
    (println (format "Trips: %d" (count detailed)))
    (print-detailed-timetable detailed :limit 100)))

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

(def create-trip-stops-ddl
  "CREATE TABLE IF NOT EXISTS scheduled_trip_stops (
    id SERIAL PRIMARY KEY,
    trip_date DATE NOT NULL,
    journey_id VARCHAR(255) NOT NULL,
    private_code VARCHAR(50),
    sequence_nr INTEGER NOT NULL,
    stop_ref VARCHAR(255),
    stop_name VARCHAR(255),
    arrival_time TIME,
    departure_time TIME,
    arrival_date DATE,
    departure_date DATE,
    for_boarding BOOLEAN DEFAULT TRUE,
    for_alighting BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  );
  CREATE INDEX IF NOT EXISTS idx_trip_stops_journey
    ON scheduled_trip_stops(trip_date, journey_id);
  CREATE INDEX IF NOT EXISTS idx_trip_stops_stop
    ON scheduled_trip_stops(stop_ref, trip_date);")

(defn trip->sql-insert [trip]
  (format "INSERT INTO scheduled_trips (trip_date, day_of_week, departure_time, arrival_time, journey_id, private_code, journey_name, line_ref, operator_ref, from_stop_ref, to_stop_ref, stop_count) VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', %d);"
          (:trip-date trip)
          (some-> (:day-of-week trip) .name)
          (:departure-time trip)
          (:arrival-time trip)
          (:journey-id trip)
          (or (:private-code trip) "")
          (or (:journey-name trip) "")
          (or (:line-ref trip) "")
          (or (:operator-ref trip) "")
          (or (:from-stop-ref trip) "")
          (or (:to-stop-ref trip) "")
          (or (:stop-count trip) 0)))

(defn stop->sql-insert [stop]
  (format "INSERT INTO scheduled_trip_stops (trip_date, journey_id, private_code, sequence_nr, stop_ref, stop_name, arrival_time, departure_time, arrival_date, departure_date, for_boarding, for_alighting) VALUES ('%s', '%s', '%s', %d, '%s', '%s', %s, %s, %s, %s, %s, %s);"
          (:trip-date stop)
          (:journey-id stop)
          (or (:private-code stop) "")
          (:sequence stop)
          (or (:stop-ref stop) "")
          (or (:stop-name stop) "")
          (if (:arrival-time stop) (format "'%s'" (:arrival-time stop)) "NULL")
          (if (:departure-time stop) (format "'%s'" (:departure-time stop)) "NULL")
          (if (:arrival-date stop) (format "'%s'" (:arrival-date stop)) "NULL")
          (if (:departure-date stop) (format "'%s'" (:departure-date stop)) "NULL")
          (:for-boarding stop)
          (:for-alighting stop)))
