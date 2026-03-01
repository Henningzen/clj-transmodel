;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/netex/registry.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025 - 2026
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.netex.registry
  "In-memory shared data registry"
  (:require [jansenh.transmodel.netex.extract :as extract]
            [jansenh.transmodel.parser.core :as parser]
            [clojure.tools.logging :as log]))

;;
;;   Transmodel NeTEx shared data registry
;;   -------------------------------------
;;
;;   The registry provides a atom based database holding extracted NeTEx
;;   entities, selected for the current needs of the transmodel application
;;   scope.
;;
;;   NOTE: I have not made a clear separation between what is shared_data
;;         and regulrar Line data file.
;;
;;   authors:   Henning Jansen - henning.jansen@jansenh.no;
;;   since:     0.2.0   2025-08-15
;;   version:   0.2.1   2026-03-01
;; -----------------------------------------------------------------------------

(defonce ^:private reg (atom {}))

(defn get-all [] @reg)

(defn reset-registry! []
  (clojure.core/reset! reg {}))

(defn- register! [k v]
  (swap! reg assoc k v)
  (let [count-str (cond
                    (map? v) (count v)
                    (seqable? v) (count v)
                    :else 1)]
    (log/infof "Registered %d %s" count-str (name k))))

;; ============================================================================
;; Lookups — shared data
;; ============================================================================

(defn operator        [id] (get-in @reg [:operators id]))
(defn day-type        [id] (get-in @reg [:day-types id]))
(defn operating-period [id] (get-in @reg [:operating-periods id]))
(defn stop-point      [id] (get-in @reg [:stop-points id]))

(defn stop-name
  "Resolve a ScheduledStopPoint ID to its human-readable name"
  [id]
  (:name (stop-point id)))

(defn all-stop-names
  "Map of ScheduledStopPoint ID → name, for bulk resolution"
  []
  (->> (:stop-points @reg)
       (reduce-kv (fn [m id sp] (assoc m id (:name sp))) {})))

(defn day-type-assignments [] (get @reg :assignments))

;; ============================================================================
;; Lookups — line data
;; ============================================================================

(defn service-journey [id] (get-in @reg [:service-journeys id]))
(defn interchange     [id] (get-in @reg [:interchanges id]))
(defn journey-pattern [id] (get-in @reg [:journey-patterns id]))
(defn line            [id] (get-in @reg [:lines id]))

(defn all-interchanges     [] (vals (get @reg :interchanges {})))
(defn all-service-journeys [] (vals (get @reg :service-journeys {})))
(defn all-journey-patterns [] (vals (get @reg :journey-patterns {})))
(defn all-lines            [] (vals (get @reg :lines {})))

;; ============================================================================
;; Derived lookups — cross-referencing
;; ============================================================================

(defn spijp-index
  "Build index: StopPointInJourneyPattern ID → stop info map.
   Cached in registry after first call per line load."
  []
  (or (get @reg :spijp-index)
      (let [idx (->> (all-journey-patterns)
                     (mapcat :stops)
                     (reduce (fn [m stop] (assoc m (:ID stop) stop)) {}))]
        (swap! reg assoc :spijp-index idx)
        idx)))

(defn resolve-stop-ref
  "Resolve StopPointInJourneyPattern ID → ScheduledStopPoint ID"
  [spijp-id]
  (:scheduled-stop-ref (get (spijp-index) spijp-id)))

(defn resolve-stop-name
  "Resolve StopPointInJourneyPattern ID → human-readable stop name"
  [spijp-id]
  (some-> (resolve-stop-ref spijp-id) stop-name))

;; ============================================================================
;; Calendar resolution
;; ============================================================================

(defn day-type-periods [day-type-id]
  (->> (day-type-assignments)
       (filter #(= (:day-type-ref %) day-type-id))
       (keep :operating-period-ref)
       (map operating-period)
       (remove nil?)))

(defn resolve-day-type [day-type-id]
  (when-let [dt (day-type day-type-id)]
    (assoc dt :operating-periods (day-type-periods day-type-id))))

;; ============================================================================
;; Loading data
;; ============================================================================

(defn load-shared-data! [pub-del]
  (register! :operators (extract/all-operators pub-del))
  (register! :day-types (extract/all-day-types pub-del))
  (register! :operating-periods (extract/all-operating-periods pub-del))
  (register! :assignments (extract/all-day-type-assignments pub-del))
  (register! :stop-points (extract/all-scheduled-stop-points pub-del))
  (log/info "✓ Shared data loaded")
  :loaded)

(defn load-line-data!
  "Load all line-level data: journeys, patterns, interchanges, lines"
  [pub-del]
  ;; Clear previous line data + cached indexes
  (swap! reg dissoc :service-journeys :interchanges
         :journey-patterns :lines :spijp-index)
  (register! :service-journeys (extract/all-service-journeys pub-del))
  (register! :journey-patterns (extract/all-journey-patterns pub-del))
  (register! :interchanges (extract/all-interchanges pub-del))
  (register! :lines (extract/all-lines pub-del))
  (log/info "✓ Line data loaded")
  :loaded)

(defn load-file!
  "Load a shared data XML file"
  [path]
  (log/infof "Loading shared data: %s" path)
  (when-let [pd (parser/parse-xml-file path)]
    (load-shared-data! pd)))

(defn load-line-file!
  "Load a line/timetable XML file"
  [path]
  (log/infof "Loading line data: %s" path)
  (when-let [pd (parser/parse-xml-file path)]
    (load-line-data! pd)))

;; Keep backward compat alias
#_(def load-service-journeys! load-line-data!)

(defn stats []
  (let [s @reg]
    {:operators (count (:operators s))
     :day-types (count (:day-types s))
     :operating-periods (count (:operating-periods s))
     :assignments (count (:assignments s))
     :stop-points (count (:stop-points s))
     :service-journeys (count (:service-journeys s))
     :journey-patterns (count (:journey-patterns s))
     :interchanges (count (:interchanges s))
     :lines (count (:lines s))}))
