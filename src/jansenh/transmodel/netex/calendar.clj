;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/netex/calendar.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025 - 2026
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.netex.calendar
  "Parse NeTEx ServiceCalendar: DayTypes, OperatingPeriods, Assignments.
   Expand to concrete operating dates for timetable generation."
  (:require [clojure.string :as str]
            [jansenh.transmodel.parser.xml :as x])
  (:import [java.time LocalDate DayOfWeek]
           [java.time.format DateTimeParseException]))

;;
;;   NeTEx ServiceCalendar: DayTypes, OperatingPeriods, Assignments.
;;   ---------------------------------------------------------------
;;
;;   Expand to concrete operating dates for timetable generation. This
;;   namespace produces application specific generated Calendar indexes for
;;   generating timetables.
;;
;;   authors:   Henning Jansen - henning.jansen@jansenh.no;
;;   since:     0.2.0   2025-08-15
;;   version:   0.2.2   2026-03-01
;; -----------------------------------------------------------------------------


;; Constants - calendar types tied to NeTEx
(def day-of-week-mapping
  {"Monday"    DayOfWeek/MONDAY
   "Tuesday"   DayOfWeek/TUESDAY
   "Wednesday" DayOfWeek/WEDNESDAY
   "Thursday"  DayOfWeek/THURSDAY
   "Friday"    DayOfWeek/FRIDAY
   "Saturday"  DayOfWeek/SATURDAY
   "Sunday"    DayOfWeek/SUNDAY})

(def weekdays #{DayOfWeek/MONDAY DayOfWeek/TUESDAY DayOfWeek/WEDNESDAY
                DayOfWeek/THURSDAY DayOfWeek/FRIDAY})

(def weekend #{DayOfWeek/SATURDAY DayOfWeek/SUNDAY})

(def everyday (into weekdays weekend))

;; -----------------------------------------------------------------------------
;; Date Parsing

(defn parse-netex-date
  "Parse NeTEx datetime string to LocalDate.
   Handles: 2025-11-21T00:00:00 or 2025-11-21
   Also passes through LocalDate instances unchanged."
  [date-str]
  (cond
    (instance? LocalDate date-str) date-str
    (and (string? date-str) (not (str/blank? date-str)))
    (try
      (let [trimmed (str/trim date-str)
            date-part (first (str/split trimmed #"T"))]
        (LocalDate/parse date-part))
      (catch DateTimeParseException _ nil))
    :else nil))

(defn date-range
  "Generate sequence of LocalDates from start to end (inclusive)"
  [^LocalDate from ^LocalDate to]
  (when (and from to (not (.isAfter from to)))
    (->> (iterate #(.plusDays ^LocalDate % 1) from)
         (take-while #(not (.isAfter ^LocalDate % to))))))

;; -----------------------------------------------------------------------------
;; DaysOfWeek Parsing

(defn parse-days-of-week
  "Parse NeTEx DaysOfWeek string to set of DayOfWeek."
  [days-str]
  (when (and days-str (not (str/blank? days-str)))
    (let [trimmed (str/trim days-str)]
      (case trimmed
        "Weekdays" weekdays
        "Weekend" weekend
        "Everyday" everyday
        (->> (str/split trimmed #"\s+")
             (keep #(get day-of-week-mapping %))
             set)))))

;; -----------------------------------------------------------------------------
;; XML Element Parsers (for raw XML path)

(defn- parse-day-type-xml
  "Parse DayType from raw XML element"
  [elem]
  (let [properties (x/find-child elem "properties")
        property-of-day (when properties (x/find-child properties "PropertyOfDay"))
        days-str (when property-of-day (x/child-text property-of-day "DaysOfWeek"))]
    {:id (x/entity-id elem)
     :version (x/entity-version elem)
     :days-of-week (parse-days-of-week days-str)
     :days-of-week-raw days-str}))

(defn- parse-operating-period-xml
  "Parse OperatingPeriod from raw XML element"
  [elem]
  {:id (x/entity-id elem)
   :version (x/entity-version elem)
   :from-date (parse-netex-date (x/child-text elem "FromDate"))
   :to-date (parse-netex-date (x/child-text elem "ToDate"))})

(defn- parse-day-type-assignment-xml
  "Parse DayTypeAssignment from raw XML element"
  [elem]
  (let [available-text (x/child-text elem "isAvailable")]
    {:id (x/entity-id elem)
     :order (some-> (get-in elem [:attrs :order]) parse-long)
     :operating-period-ref (some-> (x/find-child elem "OperatingPeriodRef") x/entity-ref)
     :day-type-ref (some-> (x/find-child elem "DayTypeRef") x/entity-ref)
     :date (parse-netex-date (x/child-text elem "Date"))
     :is-available (if available-text
                     (not= "false" available-text)
                     true)}))

;; -----------------------------------------------------------------------------
;; Calendar Index Builder

(defn- collect-from-xml
  "Walk raw XML tree and collect all calendar elements"
  [xml-root]
  {:day-types (mapv parse-day-type-xml
                    (x/find-all-deep xml-root "DayType"))
   :operating-periods (mapv parse-operating-period-xml
                            (x/find-all-deep xml-root "OperatingPeriod"))
   :assignments (mapv parse-day-type-assignment-xml
                      (x/find-all-deep xml-root "DayTypeAssignment"))})

(defn- collect-from-registry
  "Convert pre-extracted registry data to calendar format.
   Registry maps use :ID, :from-date (string), :to-date (string), etc."
  [registry-data]
  (let [day-types (:day-types registry-data)
        operating-periods (:operating-periods registry-data)
        assignments (:assignments registry-data)]
    {:day-types
     (->> (vals day-types)
          (mapv (fn [dt]
                  #_{:id (:ID dt)
                   :days-of-week nil
                   :days-of-week-raw nil}
                  ;; Parse it:
                  {:id (:id dt)
                   :days-of-week (parse-days-of-week (:days-of-week-raw dt))
                   :days-of-week-raw (:days-of-week-raw dt)})))

     :operating-periods
     (->> (vals operating-periods)
          (mapv (fn [op]
                  {:id (:id op)
                   :from-date (parse-netex-date (:from-date op))
                   :to-date (parse-netex-date (:to-date op))})))

     :assignments
     (->> assignments
          (mapv (fn [a]
                  {:day-type-ref (:day-type-ref a)
                   :operating-period-ref (:operating-period-ref a)
                   :date nil
                   :is-available true})))}))

(defn- is-registry-data?
  "Detect whether input is registry data (flat map) vs raw XML (has :tag)"
  [data]
  (and (map? data)
       (contains? data :assignments)
       (not (contains? data :tag))))

(defn- link-calendar
  "Common logic: link parsed day-types, operating-periods, assignments into index"
  [{:keys [day-types operating-periods assignments]}]
  (let [op-periods-index (reduce #(assoc %1 (:id %2) %2) {} operating-periods)

        day-types-index (reduce #(assoc %1 (:id %2)
                                        (assoc %2 :operating-periods []))
                                {} day-types)

        day-types-with-periods
        (reduce
         (fn [dt-index assignment]
           (let [{:keys [day-type-ref operating-period-ref is-available]} assignment
                 period (get op-periods-index operating-period-ref)]
             (if (and day-type-ref period (not (false? is-available)))
               (update-in dt-index [day-type-ref :operating-periods]
                          (fnil conj [])
                          {:from (:from-date period)
                           :to (:to-date period)})
               dt-index)))
         day-types-index
         assignments)]

    {:day-types day-types-with-periods
     :operating-periods op-periods-index
     :stats {:day-type-count (count day-types)
             :period-count (count operating-periods)
             :assignment-count (count assignments)}}))

(defn build-calendar-index
  "Build complete calendar index.

   Accepts either:
   - Raw parsed XML (clojure.data.xml structure from shared_data.xml)
   - Registry data map from (reg/get-all)

   Returns:
   {:day-types {id -> {:id, :days-of-week, :operating-periods [{:from :to}...]}}
    :operating-periods {id -> {:from-date :to-date}}
    :stats {...}}"
  [data]
  (let [parsed (if (is-registry-data? data)
                 (collect-from-registry data)
                 (collect-from-xml data))]
    (link-calendar parsed)))

;; -----------------------------------------------------------------------------
;; Date Expansion - Core Business Logic

(defn expand-day-type
  "Expand a DayType to concrete dates within a query period."
  [day-type ^LocalDate query-from ^LocalDate query-to]
  (let [{:keys [days-of-week operating-periods]} day-type

        ranges (if (seq operating-periods)
                 (for [{:keys [from to]} operating-periods
                       :when (and from to)]
                   {:from (if (.isBefore from query-from) query-from from)
                    :to (if (.isAfter to query-to) query-to to)})
                 [])]

    (->> ranges
         (mapcat (fn [{:keys [from to]}] (date-range from to)))
         (filter (fn [^LocalDate date]
                   (if (seq days-of-week)
                     (contains? days-of-week (.getDayOfWeek date))
                     true)))
         set
         (sort-by identity)
         vec)))

(defn get-operating-dates
  "Get all dates a DayType operates within a period."
  [calendar-index day-type-id ^LocalDate from-date ^LocalDate to-date]
  (if-let [day-type (get-in calendar-index [:day-types day-type-id])]
    (expand-day-type day-type from-date to-date)
    []))

;; -----------------------------------------------------------------------------
;; Convenience Functions

(defn weeks-ahead [n]
  (let [today (LocalDate/now)]
    {:from today :to (.plusWeeks today n)}))

(defn describe-day-type
  [calendar-index day-type-id]
  (when-let [dt (get-in calendar-index [:day-types day-type-id])]
    (let [days (:days-of-week dt)
          periods (:operating-periods dt)]
      {:id day-type-id
       :pattern (cond
                  (= days weekdays) "Weekdays (Mon-Fri)"
                  (= days weekend) "Weekend (Sat-Sun)"
                  (= days everyday) "Every day"
                  (seq days) (str/join ", " (map #(.name ^DayOfWeek %) (sort days)))
                  :else "All days in period")
       :periods (for [{:keys [from to]} periods]
                  (str from " to " to))
       :period-count (count periods)})))
