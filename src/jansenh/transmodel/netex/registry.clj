(ns jansenh.transmodel.netex.registry
  "In-memory shared data registry"
  (:require [jansenh.transmodel.netex.extract :as extract]
            [jansenh.transmodel.parser.core :as parser]
            [clojure.tools.logging :as log]))

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

;; Lookups
(defn operator [id] (get-in @reg [:operators id]))
(defn day-type [id] (get-in @reg [:day-types id]))
(defn operating-period [id] (get-in @reg [:operating-periods id]))
(defn stop-point [id] (get-in @reg [:stop-points id]))
(defn service-journey [id] (get-in @reg [:service-journeys id]))
(defn interchange [id] (get-in @reg [:interchanges id]))

(defn day-type-assignments [] (get @reg :assignments))
(defn all-interchanges [] (vals (get @reg :interchanges {})))
(defn all-service-journeys [] (vals (get @reg :service-journeys {})))

;; Calendar resolution
(defn day-type-periods [day-type-id]
  (->> (day-type-assignments)
       (filter #(= (:day-type-ref %) day-type-id))
       (keep :operating-period-ref)
       (map operating-period)
       (remove nil?)))

(defn resolve-day-type [day-type-id]
  (when-let [dt (day-type day-type-id)]
    (assoc dt :operating-periods (day-type-periods day-type-id))))

;; Loading
(defn load-shared-data! [pub-del]
  "Load shared data from shared_data.xml"
  (register! :operators (extract/all-operators pub-del))
  (register! :day-types (extract/all-day-types pub-del))
  (register! :operating-periods (extract/all-operating-periods pub-del))
  (register! :assignments (extract/all-day-type-assignments pub-del))
  (register! :stop-points (extract/all-scheduled-stop-points pub-del))
  (log/info "✓ Shared data loaded")
  :loaded)

(defn load-service-journeys! [pub-del]
  "Load service journeys and interchanges from line file"
  (register! :service-journeys (extract/all-service-journeys pub-del))
  (register! :interchanges (extract/all-interchanges pub-del))
  (log/info "✓ Service journeys and interchanges loaded")
  :loaded)

(defn load-file! [path]
  "Load a shared data XML file"
  (log/infof "Loading shared data: %s" path)
  (when-let [pd (parser/parse-xml-file path)]
    (load-shared-data! pd)))

(defn load-line-file! [path]
  "Load a line/timetable XML file"
  (log/infof "Loading line data: %s" path)
  (when-let [pd (parser/parse-xml-file path)]
    (load-service-journeys! pd)))

(defn stats []
  "Print registry statistics"
  (let [s @reg]
    {:operators (count (:operators s))
     :day-types (count (:day-types s))
     :operating-periods (count (:operating-periods s))
     :assignments (count (:assignments s))
     :stop-points (count (:stop-points s))
     :service-journeys (count (:service-journeys s))
     :interchanges (count (:interchanges s))}))
