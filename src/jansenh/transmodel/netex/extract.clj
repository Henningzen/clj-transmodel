;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/netex/extract.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025 - 2026
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.netex.extract
  "NeTEx entity extraction"
  (:require [jansenh.transmodel.parser.xml :as x]))

;;
;;   NeTEx entity extraction
;;   -----------------------
;;
;;    Single source for all element → map conversions.
;;    We use the parser.xml.clj namespace on a clojure.xml dataset and extract
;;    a subset of NeTEx entities
;;
;;   authors:   Henning Jansen - henning.jansen@jansenh.no;
;;   since:     0.2.0   2025-08-15
;;   version:   0.2.2   2026-03-01
;; -----------------------------------------------------------------------------

;; ============================================================================
;; OPERATORS
;; ============================================================================

(defn extract-operator [elem]
  (when elem
    {:id (x/entity-id elem)
     :name (x/child-text elem "Name")
     :short-name (x/child-text elem "ShortName")}))

(defn all-operators [pub-del]
  (->> (x/find-all-deep pub-del "Operator")
       (map extract-operator)
       (map (juxt :id identity))
       (into {})))

;; ============================================================================
;; DAY TYPES
;; ============================================================================

(defn extract-day-type [elem]
  (when elem
    (let [properties (x/find-child elem "properties")
          property-of-day (when properties
                            (x/find-child properties "PropertyOfDay"))]
      {:id (x/entity-id elem)
       :name (x/child-text elem "Name")
       :properties {:days-of-week (when property-of-day
                                    (x/child-text property-of-day "DaysOfWeek"))}})))


(defn all-day-types [pub-del]
  (->> (x/find-all-deep pub-del "DayType")
       (map extract-day-type)
       (map (juxt :id identity))
       (into {})))

;; ============================================================================
;; OPERATING PERIODS
;; ============================================================================

(defn extract-operating-period [elem]
  (when elem
    {:id (x/entity-id elem)
     :from-date (x/child-text elem "FromDate")
     :to-date (x/child-text elem "ToDate")}))

(defn all-operating-periods [pub-del]
  (->> (x/find-all-deep pub-del "OperatingPeriod")
       (map extract-operating-period)
       (map (juxt :id identity))
       (into {})))

;; ============================================================================
;; DAY TYPE ASSIGNMENTS
;; ============================================================================

(defn extract-day-type-assignment [elem]
  (when elem
    (let [dt-ref (x/find-child elem "DayTypeRef")
          op-ref (x/find-child elem "OperatingPeriodRef")]
      {:day-type-ref (when dt-ref (x/entity-ref dt-ref))
       :operating-period-ref (when op-ref (x/entity-ref op-ref))})))

(defn all-day-type-assignments [pub-del]
  (->> (x/find-all-deep pub-del "DayTypeAssignment")
       (mapv extract-day-type-assignment)))

;; ============================================================================
;; SCHEDULED STOP POINTS
;; ============================================================================

(defn extract-stop-point [elem]
  (when elem
    {:id (x/entity-id elem)
     :name (x/child-text elem "Name")
     :short-name (x/child-text elem "ShortName")}))

(defn all-scheduled-stop-points [pub-del]
  (->> (x/find-all-deep pub-del "ScheduledStopPoint")
       (map extract-stop-point)
       (map (juxt :id identity))
       (into {})))

;; ============================================================================
;; LINES
;; ============================================================================

(defn extract-line [elem]
  (when elem
    (let [submode-container (x/find-child elem "TransportSubmode")]
      {:id (x/entity-id elem)
       :version (x/entity-version elem)
       :name (x/child-text elem "Name")
       :transport-mode (x/child-text elem "TransportMode")
       :transport-submode (when submode-container
                            ;; Get text of first child whatever the submode tag is
                            (some-> (:content submode-container)
                                    first
                                    :content
                                    first))
       :public-code (x/child-text elem "PublicCode")
       :private-code (x/child-text elem "PrivateCode")
       :operator-ref (some-> (x/find-child elem "OperatorRef") x/entity-ref)})))

(defn all-lines [pub-del]
  (->> (x/find-all-deep pub-del "Line")
       (map extract-line)
       (map (juxt :id identity))
       (into {})))

;; ============================================================================
;; JOURNEY PATTERNS
;; ============================================================================

(defn- extract-booking-arrangements [elem]
  (when-let [ba (x/find-child elem "BookingArrangements")]
    (let [contact (x/find-child ba "BookingContact")]
      {:contact-person (when contact (x/child-text contact "ContactPerson"))
       :phone (when contact (x/child-text contact "Phone"))
       :further-details (when contact (x/child-text contact "FurtherDetails"))
       :booking-methods (x/child-text ba "BookingMethods")
       :minimum-booking-period (x/child-text ba "MinimumBookingPeriod")})))

(defn- extract-stop-in-journey-pattern [elem]
  (when elem
    (let [ssp-ref (x/find-child elem "ScheduledStopPointRef")
          dd-ref (x/find-child elem "DestinationDisplayRef")
          for-boarding-text (x/child-text elem "ForBoarding")
          for-alighting-text (x/child-text elem "ForAlighting")]
      {:id (x/entity-id elem)
       :order (x/entity-order elem)
       :scheduled-stop-ref (when ssp-ref (x/entity-ref ssp-ref))
       :for-boarding (if (nil? for-boarding-text) true (= "true" for-boarding-text))
       :for-alighting (if (nil? for-alighting-text) true (= "true" for-alighting-text))
       :destination-display-ref (when dd-ref (x/entity-ref dd-ref))
       :booking-arrangements (extract-booking-arrangements elem)})))

(defn extract-journey-pattern [elem]
  (when elem
    (let [route-ref (x/find-child elem "RouteRef")
          stops-container (x/find-child elem "pointsInSequence")
          stops (->> (x/find-children stops-container "StopPointInJourneyPattern")
                     (map extract-stop-in-journey-pattern)
                     (sort-by :order)
                     vec)]
      {:id (x/entity-id elem)
       :version (x/entity-version elem)
       :name (x/child-text elem "Name")
       :route-ref (when route-ref (x/entity-ref route-ref))
       :stops stops})))

(defn all-journey-patterns [pub-del]
  (->> (x/find-all-deep pub-del "JourneyPattern")
       (map extract-journey-pattern)
       (map (juxt :id identity))
       (into {})))

;; ============================================================================
;; SERVICE JOURNEYS
;; ============================================================================

(defn- extract-passing-time [elem]
  (when elem
    (let [stop-ref (x/find-child elem "StopPointInJourneyPatternRef")]
      {:id (x/entity-id elem)
       :stop-point-ref (when stop-ref (x/entity-ref stop-ref))
       :departure-time (x/child-text elem "DepartureTime")
       :arrival-time (x/child-text elem "ArrivalTime")
       :departure-day-offset (some-> (x/child-text elem "DepartureDayOffset") parse-long)
       :arrival-day-offset (some-> (x/child-text elem "ArrivalDayOffset") parse-long)})))

(defn- extract-flexible-service-properties [elem]
  (when elem
    (let [contact (x/find-child elem "BookingContact")]
      {:id (x/entity-id elem)
       :contact-person (when contact (x/child-text contact "ContactPerson"))
       :phone (when contact (x/child-text contact "Phone"))
       :further-details (when contact (x/child-text contact "FurtherDetails"))
       :booking-methods (x/child-text elem "BookingMethods")
       :book-when (x/child-text elem "BookWhen")
       :latest-booking-time (x/child-text elem "LatestBookingTime")})))

(defn extract-service-journey [elem]
  (when elem
    (let [day-types-container (x/find-child elem "dayTypes")
          passing-times-container (x/find-child elem "passingTimes")
          jp-ref (x/find-child elem "JourneyPatternRef")
          op-ref (x/find-child elem "OperatorRef")
          ln-ref (x/find-child elem "LineRef")
          flex-props (x/find-child elem "FlexibleServiceProperties")

          passing-times (->> (x/find-children passing-times-container
                                              "TimetabledPassingTime")
                             (map extract-passing-time)
                             vec)]
      {:id (x/entity-id elem)
       :version (x/entity-version elem)
       :name (x/child-text elem "Name")
       :private-code (x/child-text elem "PrivateCode")
       :day-type-refs (->> (x/find-children day-types-container "DayTypeRef")
                           (map x/entity-ref)
                           (remove nil?)
                           set)
       :journey-pattern-ref (when jp-ref (x/entity-ref jp-ref))
       :operator-ref (when op-ref (x/entity-ref op-ref))
       :line-ref (when ln-ref (x/entity-ref ln-ref))
       :passing-times passing-times
       :flexible-service (when flex-props
                           (extract-flexible-service-properties flex-props))})))

(defn all-service-journeys [pub-del]
  (->> (x/find-all-deep pub-del "ServiceJourney")
       (map extract-service-journey)
       (map (juxt :id identity))
       (into {})))

;; ============================================================================
;; SERVICE JOURNEY INTERCHANGES
;; ============================================================================

(defn extract-interchange [elem]
  (when elem
    {:id (x/entity-id elem)
     :version (x/entity-version elem)
     :stay-seated (= "true" (x/child-text elem "StaySeated"))
     :guaranteed (= "true" (x/child-text elem "Guaranteed"))
     :from-point-ref (some-> (x/find-child elem "FromPointRef") x/entity-ref)
     :to-point-ref (some-> (x/find-child elem "ToPointRef") x/entity-ref)
     :from-journey (some-> (x/find-child elem "FromJourneyRef") x/entity-ref)
     :to-journey (some-> (x/find-child elem "ToJourneyRef") x/entity-ref)}))

(defn all-interchanges [pub-del]
  (->> (x/find-all-deep pub-del "ServiceJourneyInterchange")
       (map extract-interchange)
       (map (juxt :id identity))
       (into {})))
