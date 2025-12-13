(ns jansenh.transmodel.generator.timetable
  "Generate flat timetable from NeTEx data for SQL export"
  (:require [jansenh.transmodel.parser.calendar :as cal]
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
       (filter #(or (= (:tag %) tag)fe) date)]
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
