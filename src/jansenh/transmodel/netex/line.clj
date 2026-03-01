;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/netex/line.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025 - 2026
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.netex.line
  "Parse Line files: ServiceJourneys, Interchanges, Roundtrips"
  (:require [jansenh.transmodel.parser.core :as parser]
            [jansenh.transmodel.netex.registry :as reg]
            [clojure.string :as str]))

;;
;;   Transmodel NeTEx Lines parser
;;   -----------------------------
;;
;;   Parse Line files:
;;   - ServiceJourneys
;;   - Interchanges
;;   - Roundtrips
;;
;;   NOTE: This namespace has not yet been refactored to the 0.2.1 updated
;;         strategy with a shared registry, extract namespaces.
;;
;;   authors:   Henning Jansen - henning.jansen@jansenh.no;
;;   since:     0.2.0   2025-08-15
;;   version:   0.2.1   2026-03-01
;; -----------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; XML helpers (duplicated for self-containment)
;; ---------------------------------------------------------------------------

(defn- tag= [el n] (when-let [t (:tag el)] (= (name t) n)))
(defn- children [el n] (when el (->> (:content el) (filter map?) (filter #(tag= % n)))))
(defn- child [el n] (first (children el n)))
(defn- txt [el n] (some-> (child el n) :content first (as-> x (when (string? x) (str/trim x)))))
(defn- eid [el] (get-in el [:attrs :id]))
(defn- eref [el] (get-in el [:attrs :ref]))

;; ---------------------------------------------------------------------------
;; PassingTimes
;; ---------------------------------------------------------------------------

(defn- parse-passing-time [pt]
  {:id              (eid pt)
   :stop-ref        (eref (child pt "StopPointInJourneyPatternRef"))
   :arrival         (txt pt "ArrivalTime")
   :departure       (txt pt "DepartureTime")
   :arrival-offset  (txt pt "ArrivalDayOffset")
   :departure-offset (txt pt "DepartureDayOffset")})

;; ---------------------------------------------------------------------------
;; ServiceJourneys
;; ---------------------------------------------------------------------------

(defn- parse-service-journey [sj]
  {:id               (eid sj)
   :name             (txt sj "Name")
   :private-code     (txt sj "PrivateCode")
   :day-type-refs    (->> (child sj "dayTypes") (children "DayTypeRef") (mapv eref))
   :pattern-ref      (eref (child sj "JourneyPatternRef"))
   :operator-ref     (eref (child sj "OperatorRef"))
   :line-ref         (eref (child sj "LineRef"))
   :passing-times    (->> (child sj "passingTimes")
                          (children "TimetabledPassingTime")
                          (mapv parse-passing-time))})

(defn extract-service-journeys [timetable-frame]
  (->> (children (child timetable-frame "vehicleJourneys") "ServiceJourney")
       (map parse-service-journey)))

;; ---------------------------------------------------------------------------
;; ServiceJourneyInterchange
;; ---------------------------------------------------------------------------

(defn- parse-interchange [ic]
  {:id           (eid ic)
   :stay-seated  (= "true" (txt ic "StaySeated"))
   :guaranteed   (= "true" (txt ic "Guaranteed"))
   :from-point   (eref (child ic "FromPointRef"))
   :to-point     (eref (child ic "ToPointRef"))
   :from-journey (eref (child ic "FromJourneyRef"))
   :to-journey   (eref (child ic "ToJourneyRef"))})

(defn extract-interchanges [timetable-frame]
  (->> (children (child timetable-frame "journeyInterchanges") "ServiceJourneyInterchange")
       (map parse-interchange)))

;; ---------------------------------------------------------------------------
;; Roundtrip Chain Building
;; ---------------------------------------------------------------------------

(defn- build-graph
  "Build adjacency: from-journey -> [to-journey] for StaySeated=true"
  [interchanges]
  (->> interchanges
       (filter :stay-seated)
       (reduce (fn [g {:keys [from-journey to-journey]}]
                 (update g from-journey (fnil conj []) to-journey))
               {})))

(defn- find-chain-starts
  "Journeys that start chains (from but not to)"
  [interchanges]
  (let [stay-seated (filter :stay-seated interchanges)
        froms (set (map :from-journey stay-seated))
        tos (set (map :to-journey stay-seated))]
    (clojure.set/difference froms tos)))

(defn- follow-chain
  "Follow chain from start journey"
  [graph start]
  (loop [cur start, chain [start], seen #{start}]
    (if-let [nxt (first (remove seen (get graph cur)))]
      (recur nxt (conj chain nxt) (conj seen nxt))
      chain)))

(defn build-roundtrip-chains
  "Extract all StaySeated chains"
  [interchanges]
  (let [graph (build-graph interchanges)
        starts (find-chain-starts interchanges)]
    (->> starts
         (map #(follow-chain graph %))
         (filter #(> (count %) 1))
         (sort-by count >))))

;; ---------------------------------------------------------------------------
;; Roundtrip Summary
;; ---------------------------------------------------------------------------

(defn summarize-roundtrip
  "Human-readable roundtrip from chain + journey index"
  [chain journeys-idx]
  (let [legs (mapv #(get journeys-idx %) chain)]
    {:journey-count (count chain)
     :journey-ids   chain
     :legs          (mapv (fn [j]
                            {:id        (:id j)
                             :name      (:name j)
                             :code      (:private-code j)
                             :departure (-> j :passing-times first :departure)
                             :arrival   (-> j :passing-times last :arrival)
                             :stops     (count (:passing-times j))})
                          legs)
     :start-time    (-> legs first :passing-times first :departure)
     :end-time      (-> legs last :passing-times last :arrival)}))

;; ---------------------------------------------------------------------------
;; Main API
;; ---------------------------------------------------------------------------

(defn parse-line-file
  "Parse a line file, return journeys + interchanges + chains"
  [file-path]
  (when-let [pub-del (parser/parse-xml-file file-path)]
    (let [cf (-> (child pub-del "dataObjects") (children "CompositeFrame") first)
          tf (-> (child cf "frames") (children "TimetableFrame") first)

          journeys (extract-service-journeys tf)
          journeys-idx (reduce (fn [m j] (assoc m (:id j) j)) {} journeys)
          interchanges (extract-interchanges tf)
          chains (build-roundtrip-chains interchanges)]

      {:journeys      journeys-idx
       :interchanges  interchanges
       :chains        chains
       :roundtrips    (mapv #(summarize-roundtrip % journeys-idx) chains)})))

(defn standalone-journeys
  "Journeys not in any StaySeated chain"
  [{:keys [journeys chains]}]
  (let [chained-ids (set (mapcat identity chains))]
    (->> (vals journeys)
         (remove #(chained-ids (:id %))))))

;; ---------------------------------------------------------------------------
;; With Registry Resolution
;; ---------------------------------------------------------------------------

(defn resolve-journey
  "Enrich journey with registry data"
  [journey]
  (-> journey
      (assoc :operator (reg/operator (:operator-ref journey)))
      (assoc :day-types (mapv reg/resolve-day-type (:day-type-refs journey)))))

(defn resolve-roundtrip
  "Enrich roundtrip with registry data"
  [roundtrip journeys-idx]
  (let [resolved-legs (mapv #(resolve-journey (get journeys-idx %))
                            (:journey-ids roundtrip))]
    (assoc roundtrip :resolved-legs resolved-legs)))
