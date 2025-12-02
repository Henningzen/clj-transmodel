;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/parser/calendar.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.parser.calendar
  "Parse NeTEx ServiceCalendar: DayTypes, OperatingPeriods, Assignments.
   Expand to concrete operating dates for timetable generation."
  (:require [clojure.string :as str])
  (:import [java.time LocalDate LocalDateTime DayOfWeek]
           [java.time.format DateTimeFormatter DateTimeParseException]))

;; =============================================================================
;; Constants
;; =============================================================================

(def netex-ns "xmlns.http%3A%2F%2Fwww.netex.org.uk%2Fnetex")
(defn nkw [local-name] (keyword netex-ns local-name))

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

;; =============================================================================
;; XML Navigation Helpers
;; =============================================================================

(defn find-child
  "Find first child element with given tag"
  [element tag]
  (->> (:content element)
       (filter #(= (:tag %) tag))
       first))

(defn find-children
  "Find all child elements with given tag"
  [element tag]
  (->> (:content element)
       (filter #(= (:tag %) tag))))

(defn text-content
  "Get text content of element"
  [element]
  (->> (:content element)
       (filter string?)
       (apply str)
       str/trim))

(defn attr
  "Get attribute value"
  [element attr-name]
  (get (:attrs element) attr-name))

;; =============================================================================
;; Date Parsing
;; =============================================================================

(defn parse-netex-date
  "Parse NeTEx datetime string to LocalDate.
   Handles: 2025-11-21T00:00:00 or 2025-11-21"
  [date-str]
  (when (and date-str (not (str/blank? date-str)))
    (try
      (let [trimmed (str/trim date-str)
            ;; Take just the date part before T
            date-part (first (str/split trimmed #"T"))]
        (LocalDate/parse date-part))
      (catch DateTimeParseException _ nil))))

(defn date-range
  "Generate sequence of LocalDates from start to end (inclusive)"
  [^LocalDate from ^LocalDate to]
  (when (and from to (not (.isAfter from to)))
    (->> (iterate #(.plusDays ^LocalDate % 1) from)
         (take-while #(not (.isAfter ^LocalDate % to))))))

;; =============================================================================
;; DaysOfWeek Parsing
;; =============================================================================

(defn parse-days-of-week
  "Parse NeTEx DaysOfWeek string to set of DayOfWeek.
   
   Examples:
     'Monday Tuesday Wednesday' -> #{MONDAY TUESDAY WEDNESDAY}
     'Weekdays' -> #{MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY}
     'Weekend' -> #{SATURDAY SUNDAY}
     'Everyday' -> all days"
  [days-str]
  (when (and days-str (not (str/blank? days-str)))
    (let [trimmed (str/trim days-str)]
      (case trimmed
        "Weekdays" weekdays
        "Weekend" weekend
        "Everyday" everyday
        ;; Parse individual days
        (->> (str/split trimmed #"\s+")
             (keep #(get day-of-week-mapping %))
             set)))))

;; =============================================================================
;; Element Parsers
;; =============================================================================

(defn parse-day-type
  "Parse DayType element"
  [elem]
  (let [id (attr elem :id)
        version (attr elem :version)
        properties (find-child elem (nkw "properties"))
        property-of-day (when properties 
                          (find-child properties (nkw "PropertyOfDay")))
        days-elem (when property-of-day
                    (find-child property-of-day (nkw "DaysOfWeek")))
        days-str (when days-elem (text-content days-elem))]
    {:id id
     :version version
     :days-of-week (parse-days-of-week days-str)
     :days-of-week-raw days-str}))

(defn parse-operating-period
  "Parse OperatingPeriod element"
  [elem]
  (let [from-elem (find-child elem (nkw "FromDate"))
        to-elem (find-child elem (nkw "ToDate"))]
    {:id (attr elem :id)
     :version (attr elem :version)
     :from-date (parse-netex-date (text-content from-elem))
     :to-date (parse-netex-date (text-content to-elem))}))

(defn parse-day-type-assignment
  "Parse DayTypeAssignment element"
  [elem]
  (let [op-ref-elem (find-child elem (nkw "OperatingPeriodRef"))
        dt-ref-elem (find-child elem (nkw "DayTypeRef"))
        date-elem (find-child elem (nkw "Date"))
        available-elem (find-child elem (nkw "isAvailable"))]
    {:id (attr elem :id)
     :order (some-> (attr elem :order) parse-long)
     :operating-period-ref (when op-ref-elem (attr op-ref-elem :ref))
     :day-type-ref (when dt-ref-elem (attr dt-ref-elem :ref))
     :date (when date-elem (parse-netex-date (text-content date-elem)))
     :is-available (if available-elem
                     (not= "false" (text-content available-elem))
                     true)}))

;; =============================================================================
;; Tree Walking - Find All Calendar Elements
;; =============================================================================

(defn collect-calendar-elements
  "Walk XML tree and collect all calendar-related elements."
  [xml-root]
  (let [day-types (atom [])
        operating-periods (atom [])
        assignments (atom [])
        
        ;; All possible tag variations
        day-type-tags #{(nkw "DayType") :DayType}
        op-period-tags #{(nkw "OperatingPeriod") :OperatingPeriod}
        assignment-tags #{(nkw "DayTypeAssignment") :DayTypeAssignment}
        
        walk (fn walk [elem]
               (when (map? elem)
                 (let [tag (:tag elem)]
                   (cond
                     (or (day-type-tags tag)
                         (= "DayType" (some-> tag name)))
                     (swap! day-types conj elem)
                     
                     (or (op-period-tags tag)
                         (= "OperatingPeriod" (some-> tag name)))
                     (swap! operating-periods conj elem)
                     
                     (or (assignment-tags tag)
                         (= "DayTypeAssignment" (some-> tag name)))
                     (swap! assignments conj elem)))
                 
                 (doseq [child (:content elem)]
                   (walk child))))]
    
    (walk xml-root)
    
    {:day-types @day-types
     :operating-periods @operating-periods
     :assignments @assignments}))


;; =============================================================================
;; Calendar Index Builder
;; =============================================================================

(defn build-calendar-index
  "Build complete calendar index from parsed shared_data.xml.
   
   Returns:
   {:day-types {id -> {:id, :days-of-week, :operating-periods [{:from :to}...]}}
    :operating-periods {id -> {:from-date :to-date}}}"
  [shared-data-xml]
  (let [elements (collect-calendar-elements shared-data-xml)
        
        ;; Parse all elements
        day-types-raw (map parse-day-type (:day-types elements))
        op-periods-raw (map parse-operating-period (:operating-periods elements))
        assignments-raw (map parse-day-type-assignment (:assignments elements))
        
        ;; Index operating periods by ID
        op-periods-index (reduce #(assoc %1 (:id %2) %2) {} op-periods-raw)
        
        ;; Index day types by ID  
        day-types-index (reduce #(assoc %1 (:id %2) 
                                        (assoc %2 :operating-periods []))
                                {} day-types-raw)
        
        ;; Link assignments: add operating periods to day types
        day-types-with-periods
        (reduce
         (fn [dt-index assignment]
           (let [{:keys [day-type-ref operating-period-ref is-available]} assignment
                 period (get op-periods-index operating-period-ref)]
             (if (and day-type-ref period is-available)
               (update-in dt-index [day-type-ref :operating-periods]
                          (fnil conj [])
                          {:from (:from-date period)
                           :to (:to-date period)})
               dt-index)))
         day-types-index
         assignments-raw)]
    
    {:day-types day-types-with-periods
     :operating-periods op-periods-index
     :stats {:day-type-count (count day-types-raw)
             :period-count (count op-periods-raw)
             :assignment-count (count assignments-raw)}}))

;; =============================================================================
;; Date Expansion - Core Business Logic
;; =============================================================================

(defn expand-day-type
  "Expand a DayType to concrete dates within a query period.
   
   Parameters:
   - day-type: parsed day type map with :days-of-week and :operating-periods
   - query-from: start of query range (LocalDate)
   - query-to: end of query range (LocalDate)
   
   Logic:
   1. If day-type has operating-periods, use those as bounds
   2. If day-type has days-of-week pattern, filter to matching days
   3. If no days-of-week, include all days in the period
   
   Returns: sorted vector of LocalDate"
  [day-type ^LocalDate query-from ^LocalDate query-to]
  (let [{:keys [days-of-week operating-periods]} day-type
        
        ;; Determine effective date ranges
        ranges (if (seq operating-periods)
                 ;; Use operating periods, intersected with query range
                 (for [{:keys [from to]} operating-periods
                       :when (and from to)]
                   {:from (if (.isBefore from query-from) query-from from)
                    :to (if (.isAfter to query-to) query-to to)})
                 ;; No operating periods = no dates (or should we use query range?)
                 [])]
    
    (->> ranges
         ;; Generate all dates in each range
         (mapcat (fn [{:keys [from to]}]
                   (date-range from to)))
         ;; Filter by days-of-week if specified
         (filter (fn [^LocalDate date]
                   (if (seq days-of-week)
                     (contains? days-of-week (.getDayOfWeek date))
                     true)))  ;; No pattern = every day
         ;; Remove duplicates and sort
         set
         (sort-by identity)
         vec)))

(defn get-operating-dates
  "High-level function: get all dates a DayType operates.
   
   Parameters:
   - calendar-index: result of build-calendar-index
   - day-type-id: e.g., 'KOL:DayType:1050'
   - from-date: start of period
   - to-date: end of period
   
   Returns: vector of LocalDate, or empty vector if DayType not found"
  [calendar-index day-type-id ^LocalDate from-date ^LocalDate to-date]
  (if-let [day-type (get-in calendar-index [:day-types day-type-id])]
    (expand-day-type day-type from-date to-date)
    []))

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn weeks-ahead
  "Get date range for N weeks from today"
  [n]
  (let [today (LocalDate/now)]
    {:from today
     :to (.plusWeeks today n)}))

(defn describe-day-type
  "Human-readable description of a DayType"
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
