(ns jansenh.transmodel.api
  "API wrapper over jansenh/transmodel"
  (:require [jansenh.transmodel.parser.core :as parser]
            [jansenh.transmodel.netex.calendar :as cal]
            [jansenh.transmodel.generator.timetable :as tt]
            [jansenh.transmodel.netex.registry :as reg]
            [jansenh.transmodel.netex.line :as line]
            [jansenh.transmodel.netex.interchanges :as interchanges]
            [clojure.pprint :as pp]
            [clojure.string :as str])
  (:import [java.time LocalDate DayOfWeek]
           [java.time.format DateTimeParseException]
           [java.time DayOfWeek]))

;; ===========================================================================
;;
;; --- Define & load files
;;
;; ===========================================================================

(def shared-data-file "/home/jansenh/data/rb_norway-aggregated-netex/KOL/_KOL_shared_data.xml")

(def line-data-file "/home/jansenh/data/rb_norway-aggregated-netex/KOL/KOL_KOL-Line-8_5900_518_518.xml")

(def shared-data (parser/parse-xml-file shared-data-file))
(def line-data (parser/parse-xml-file line-data-file))

(reg/reset-registry!)
(reg/load-file! shared-data-file)
(reg/load-line-file! line-data-file)
(reg/stats)


;; Set date ranges
(def date-range (cal/weeks-ahead 6))
(def from-date (:from date-range))
(def to-date (:to date-range))

(let [cal-idx (cal/build-calendar-index (reg/get-all))]
  (tt/print-daily-detailed cal-idx "2026-03-01"))


;; ===========================================================================
;;
;; --- Understanding Lines
;;
;; ===========================================================================

(reg/all-lines)
;;(reg/operator)

;; ===========================================================================
;;
;; --- Build calendar index
;;
;; ===========================================================================

(def calendar-index (cal/build-calendar-index shared-data))
(:stats calendar-index)


;; ===========================================================================
;;
;; --- Day-types, navigating and understanding
;;
;; ===========================================================================

(defn describe-day-type
  "Describe a DayType using registry data.
   
   Returns a map with:
   - :id - the day type ID
   - :name - human name
   - :periods - vector of period date ranges as strings
   - :period-count - number of operating periods"
  [day-type-id]
  (when-let [dt (reg/day-type day-type-id)]
    (let [periods (reg/day-type-periods day-type-id)]
      {:id day-type-id
       :name (:name dt)
       :periods (mapv (fn [p]
                        (str (:from-date p) " to " (:to-date p)))
                      periods)
       :period-count (count periods)})))

(defn describe-day-type-with-pattern
  "Extended description including day-of-week pattern from calendar index"
  [calendar-index day-type-id]
  (when-let [base (describe-day-type day-type-id)]
    (if-let [cal-dt (get-in calendar-index [:day-types day-type-id])]
      (let [days (:days-of-week cal-dt)]
        (assoc base
               :day-pattern
               (cond
                 (empty? days) "Not yet resolved"
                 :else (str/join ", " (map #(.name ^DayOfWeek %) (sort days))))))
      base)))


;; Inspect a specific day type
;; (The calendar-index in shared_data.xml has DayTypes, a fundamental
;;  concept to most of our routes.)

(comment
  (cal/describe-day-type calendar-index "KOL:DayType:1050")
  (describe-day-type "KOL:DayType:1050")
  (describe-day-type-with-pattern calendar-index "KOL:DayType:1050")
  ;; --->
  )

;; ===========================================================================
;;
;; TODO : Document this insight from AI:
;;        Both produce the same calendar index structure
;;
;; ===========================================================================

;; From raw XML (backward compat):
(cal/build-calendar-index shared-data)

;; From registry:
(cal/build-calendar-index (reg/get-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
;; ===========================================================================
;;
;; -- Generate all Journeys  --- Line context
;;
;; ===========================================================================
;;
;;   The Line file (LineRef) has ServiceJourneys, the second most fundamental
;;   concept in NeTEx routes.
;;   The ServiceJourney can or can not rely on DayTypes in shared_data.xml
;;   If no DayTypes are used in ServiceJourneys, the we need a
;;   shared_flexible_data.xml which is out-of-scope.
;;
;;   The logic:
;;   + DayType with DaysOfWeek + linked OperatingPeriod = Pattern applied
;;     within that date range.
;;   + DayType without DaysOfWeek + linked OperatingPeriod = Every day in
;;     that range.
;;

;; Get service journeys from line file OBSOLETE
;; (The line.clj namespace has this function intact, bypassing the
;;  registry way of dealing with extraction)
#_(def journeys (tt/collect-service-journeys line-data))

;; Get service journeys from registry:
(def journeys (reg/all-service-journeys))
#_ (count journeys)


(defn print-timetable-today
  "Pretty-print timetable. No args = today, or pass a date string."
  ([]
   (print-timetable-today (.toString (LocalDate/now))))
  ([date]
   (let [cal-idx (cal/build-calendar-index (reg/get-all))]
     (tt/print-daily-detailed cal-idx date))))

(defn print-timetable-range
  "Pretty-print detailed timetable for a date range."
  [from-str to-str]
  (let [cal-idx (cal/build-calendar-index (reg/get-all))
        from (LocalDate/parse from-str)
        to (LocalDate/parse to-str)
        detailed (tt/generate-detailed-timetable cal-idx from to)]
    (tt/print-detailed-timetable detailed :limit 50)))


(print-timetable-today)
(print-timetable-today "2026-02-26")
(print-timetable-range "2026-03-02" "2026-03-08")


;; ===========================================================================
;;
;; Display human-readable timetable
;;
;; ===========================================================================

(comment
  
  (tt/print-timetable timetable :limit 20)
  (tt/print-timetable (tt/today calendar-index journeys))
  (tt/print-timetable (tt/tomorrow calendar-index journeys))


  ;; Generate SQL
  (println tt/create-table-ddl)
  (doseq [trip (take 10 timetable)]
    (println (tt/generate-insert-sql trip)))

  ;; ---> comment
  ;;  
  )


;; ===========================================================================
;;
;; Resolve Journeys and Roundtrips from Line data
;;
;; A roundtrip is derived from a series of ServiceJourneys connected by a
;; ServiceJourneyInterchange
;;
;; ---------  TODO: Complete the RoundTrip analyzer in explore.clj
;;
;; ===========================================================================

(def data (line/parse-line-file line-data-file))

(comment
  ;; Inspect
  (count (:journeys data))
  (count (:interchanges data))
  (count (:roundtrips data))

  ;; View roundtrips
  (first (:roundtrips data))

  ;; Standalone journeys (not in any chain)
  (count (line/standalone-journeys data))

  ;; --->
  )


;; -----------------------------------------------------------------------------
;;
;; Lines with registry resolution
;;
;; -----------------------------------------------------------------------------

(line/resolve-roundtrip (second (:roundtrips data)) (:journeys data))

(line/resolve-journey (first journeys))


;; ===========================================================================
;;
;; Resolve Interchanges
;;
;; Using the ServiceJourneyInterchange, we get tuples of ServiceJourneys,
;; interconnected with two association flags
;;   1. StaySeated [true, false]
;;   2. Guaranteed [true, false]
;;
;; ===========================================================================


(interchanges/interchange-stats)

;; Visualize a specific journey
(interchanges/visualize-journey-interchanges
 "KOL:ServiceJourney:5900_250619122707475_1002")
  
;; Visualize a specific journey
(interchanges/visualize-journey-interchanges
 "KOL:ServiceJourney:5900_250619122707482_2002")


(do 
  (let [sj (reg/service-journey "KOL:ServiceJourney:5900_250619122707475_1002")]
    (clojure.pprint/pprint (:passing-times sj)))
  
  (println "--------------------------------------------------------------------------------")
  
  (let [sj (reg/service-journey "KOL:ServiceJourney:5900_250619122707482_2002")]
    (clojure.pprint/pprint (:passing-times sj))))

  
  ;; Visualize all interchanges (first 5)
  (let [all-ic (reg/all-interchanges)]
    (doseq [ic (take 100 all-ic)]
      (interchanges/visualize-interchange ic)))

  #_(let [sj (reg/service-journey "KOL:ServiceJourney:5900_250619122707482_2002")]
    (clojure.pprint/pprint (:passing-times sj)))
  
  ;;
  ;; ------------------------------------------------------------------> comment
  ;;
  


