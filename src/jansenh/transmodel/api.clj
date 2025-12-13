(ns jansenh.transmodel.api
  "API wrapper over jansenh/transmodel"
  (:require [jansenh.transmodel.parser.core :as parser]
            [jansenh.transmodel.parser.calendar :as cal]
            [jansenh.transmodel.generator.timetable :as tt]
            [jansenh.transmodel.netex.registry :as reg]
            [jansenh.transmodel.netex.line :as line]
            #_[jansenh.transmodel.netex.explore :as exp]
            #_[jansenh.transmodel.netex.extract :as ext]
            [jansenh.transmodel.netex.interchanges :as interchanges]
            [clojure.pprint :as pp])
  (:import [java.time LocalDate]))

;; ===========================================================================
;;
;; --- Define & load files
;;
;; ===========================================================================
(def shared-data-file "/home/jansenh/data/netex/raw/KOL/_KOL_shared_data.xml")
(def line-data-file "/home/jansenh/data/netex/raw/KOL/KOL_KOL-Line-8_5900_518_518.xml")

(def shared-data (parser/parse-xml-file shared-data-file))
(def line-data (parser/parse-xml-file line-data-file))

(reg/reset-registry!)
(reg/load-file! shared-data-file)
(reg/load-line-file! line-data-file)

(println
 "\n--------------------------------------------------------------------------------\n\n"
 (reg/stats)
 "\n\n--------------------------------------------------------------------------------\n")

;; Set date ranges
(def date-range (cal/weeks-ahead 6))
(def from-date (:from date-range))
(def to-date (:to date-range))


;; ===========================================================================
;;
;; --- Build calendar index
;;
;; ===========================================================================

(def calendar-index (cal/build-calendar-index shared-data))
#_(:stats calendar-index)

;; Inspect a specific day type
;; (The calendar-index in shared_data.xml has DayTypes, a fundamental
;;  concept to most of our routes.)
#_ (cal/describe-day-type calendar-index "KOL:DayType:1050")

  
;; ===========================================================================
;;
;; -- Generate all Journeys
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

;; Get service journeys from line file
(def journeys (tt/collect-service-journeys line-data))

#_ (count journeys)

#_ (let [{:keys [from to]} (cal/weeks-ahead 6)
         timetable (tt/generate-timetable calendar-index journeys from to)]
     (tt/print-timetable timetable :limit 20))

;; Build timetable
(def timetable (tt/generate-timetable calendar-index journeys from-date to-date))

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
  


