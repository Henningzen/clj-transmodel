(ns jansenh.transmodel.netex.extract
  (:require [jansenh.transmodel.parser.utilities :as u]))

(def ^:private netex-ns :xmlns.http%3A%2F%2Fwww.netex.org.uk%2Fnetex)

(defn- ns-tag
  "Create a namespaced keyword from local tag name.
  "
  [local-name]
  (keyword (str (name netex-ns) "/" local-name)))

;; ============================================================================
;; ServiceJourneys
;; ============================================================================

(defn extract-passing-times
  "Extract arrival/departure times from a ServiceJourney's content"
  [sj-element]
  (let [passing-times-elem (u/find-child sj-element (ns-tag "passingTimes"))
        passing-time-elements (when passing-times-elem
                                (u/find-children passing-times-elem (ns-tag "TimetabledPassingTime")))]
    (->> passing-time-elements
         (map-indexed (fn [idx pt]
                        (let [stop-ref-elem (u/find-child pt (ns-tag "StopPointInJourneyPatternRef"))
                              arrival (u/get-text (u/find-child pt (ns-tag "ArrivalTime")))
                              departure (u/get-text (u/find-child pt (ns-tag "DepartureTime")))]
                          {:order (inc idx)
                           :id (u/element-id pt)
                           :stop-ref (u/element-ref stop-ref-elem)
                           :arrival-time arrival
                           :departure-time departure})))
         (into []))))

(defn extract-service-journey
  "Extract a single ServiceJourney element into a map"
  [sj-element]
  (when sj-element
    {:ID (u/element-id sj-element)
     :version (get-in sj-element [:attrs :version])
     :name (u/get-text (u/find-child sj-element (ns-tag "Name")))
     :private-code (u/get-text (u/find-child sj-element (ns-tag "PrivateCode")))
     :line-ref (u/element-ref (u/find-child sj-element (ns-tag "LineRef")))
     :journey-pattern-ref (u/element-ref (u/find-child sj-element (ns-tag "JourneyPatternRef")))
     :operator-ref (u/element-ref (u/find-child sj-element (ns-tag "OperatorRef")))
     :day-types (mapv u/element-ref 
                      (u/find-children (u/find-child sj-element (ns-tag "dayTypes")) (ns-tag "DayTypeRef")))
     :passing-times (extract-passing-times sj-element)}))

(defn all-service-journeys
  "Extract all ServiceJourney elements from publication delivery, indexed by ID"
  [pub-del]
  (let [sj-elements (u/find-all-tags pub-del (ns-tag "ServiceJourney"))]
    (->> sj-elements
         (map extract-service-journey)
         (map (juxt :ID identity))
         (into {}))))

;; ============================================================================
;; ServiceJourneyInterchanges
;; ============================================================================

(defn extract-interchange
  "Extract a single ServiceJourneyInterchange element into a map"
  [ic-element]
  (when ic-element
    {:ID (u/element-id ic-element)
     :version (get-in ic-element [:attrs :version])
     :stay-seated (= "true" (u/get-text (u/find-child ic-element (ns-tag "StaySeated"))))
     :guaranteed (= "true" (u/get-text (u/find-child ic-element (ns-tag "Guaranteed"))))
     :from-point-ref (u/element-ref (u/find-child ic-element (ns-tag "FromPointRef")))
     :to-point-ref (u/element-ref (u/find-child ic-element (ns-tag "ToPointRef")))
     :from-journey (u/element-ref (u/find-child ic-element (ns-tag "FromJourneyRef")))
     :to-journey (u/element-ref (u/find-child ic-element (ns-tag "ToJourneyRef")))}))

(defn all-interchanges
  "Extract all ServiceJourneyInterchange elements, indexed by ID"
  [pub-del]
  (let [ic-elements (u/find-all-tags pub-del (ns-tag "ServiceJourneyInterchange"))]
    (->> ic-elements
         (map extract-interchange)
         (map (juxt :ID identity))
         (into {}))))

;; ============================================================================
;; Operators
;; ============================================================================

;; (defn extract-operator
;;   "Extract a single Operator element"
;;   [op-element]
;;   (when op-element
;;     {:ID (u/element-id op-element)
;;      :name (u/get-text (u/find-child op-element (ns-tag "Name")))
;;      :short-name (u/get-text (u/find-child op-element (ns-tag "ShortName")))}))
(defn extract-operator
  "Extract a single Operator element"
  [op-element]
  (when op-element
    {:ID (u/element-id op-element)
     :name (u/tag-text (:content op-element) (ns-tag "Name"))
     :short-name (u/tag-text (:content op-element) (ns-tag "ShortName"))}))

(defn all-operators
  "Extract all Operator elements, indexed by ID"
  [pub-del]
  (let [op-elements (u/find-all-tags pub-del (ns-tag "Operator"))]
    (->> op-elements
         (map extract-operator)
         (map (juxt :ID identity))
         (into {}))))

;; ============================================================================
;; DayTypes
;; ============================================================================

;; (defn extract-day-type
;;   "Extract a single DayType element"
;;   [dt-element]
;;   (when dt-element
;;     {:ID (u/element-id dt-element)
;;      :name (u/get-text (u/find-child dt-element (ns-tag "Name")))}))

(defn extract-day-type
  "Extract a single DayType element"
  [dt-element]
  (when dt-element
    {:ID (u/element-id dt-element)
     :name (u/tag-text (:content dt-element) (ns-tag "Name"))}))

(defn all-day-types
  "Extract all DayType elements, indexed by ID"
  [pub-del]
  (let [dt-elements (u/find-all-tags pub-del (ns-tag "DayType"))]
    (->> dt-elements
         (map extract-day-type)
         (map (juxt :ID identity))
         (into {}))))

;; ============================================================================
;; OperatigPeriods
;; ============================================================================

;; (defn extract-operating-period
;;   "Extract a single OperatingPeriod element"
;;   [op-element]
;;   (when op-element
;;     {:ID (u/element-id op-element)
;;      :from-date (u/get-text (u/find-child op-element (ns-tag "FromDate")))
;;      :to-date (u/get-text (u/find-child op-element (ns-tag "ToDate")))}))
(defn extract-operating-period
  "Extract a single OperatingPeriod element"
  [op-element]
  (when op-element
    {:ID (u/element-id op-element)
     :from-date (u/tag-text (:content op-element) (ns-tag "FromDate"))
     :to-date (u/tag-text (:content op-element) (ns-tag "ToDate"))}))

(defn all-operating-periods
  "Extract all OperatingPeriod elements, indexed by ID"
  [pub-del]
  (let [op-elements (u/find-all-tags pub-del (ns-tag "OperatingPeriod"))]
    (->> op-elements
         (map extract-operating-period)
         (map (juxt :ID identity))
         (into {}))))

;; ============================================================================
;; DayTypeAssignements
;; ============================================================================

(defn extract-day-type-assignment
  "Extract a single DayTypeAssignment element"
  [dta-element]
  (when dta-element
    {:day-type-ref (u/element-ref (u/find-child dta-element (ns-tag "DayTypeRef")))
     :operating-period-ref (u/element-ref (u/find-child dta-element (ns-tag "OperatingPeriodRef")))}))

(defn all-day-type-assignments
  "Extract all DayTypeAssignment elements as a vector"
  [pub-del]
  (let [dta-elements (u/find-all-tags pub-del (ns-tag "DayTypeAssignment"))]
    (mapv extract-day-type-assignment dta-elements)))

;; ============================================================================
;; ScheduledStopPoints
;; ============================================================================

;; (defn extract-stop-point
;;   "Extract a single ScheduledStopPoint element"
;;   [sp-element]
;;   (when sp-element
;;     {:ID (u/element-id sp-element)
;;      :name (u/get-text (u/find-child sp-element (ns-tag "Name")))
;;      :short-name (u/get-text (u/find-child sp-element (ns-tag "ShortName")))}))

(defn extract-stop-point
  "Extract a single ScheduledStopPoint element"
  [sp-element]
  (when sp-element
    {:ID (u/element-id sp-element)
     :name (u/tag-text (:content sp-element) (ns-tag "Name"))
     :short-name (u/tag-text (:content sp-element) (ns-tag "ShortName"))}))

(defn all-scheduled-stop-points
  "Extract all ScheduledStopPoint elements, indexed by ID"
  [pub-del]
  (let [sp-elements (u/find-all-tags pub-del (ns-tag "ScheduledStopPoint"))]
    (->> sp-elements
         (map extract-stop-point)
         (map (juxt :ID identity))
         (into {}))))
