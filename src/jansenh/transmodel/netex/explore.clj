(ns jansenh.transmodel.netex.explore
  "Quick exploration of Line file structures

  TODO: Migrate to Notebook
  "
  (:require [jansenh.transmodel.parser.core :as parser]
            [clojure.string :as str]))

;; Reuse helpers
(defn- tag-matches? [el local-name] 
  (when-let [t (:tag el)] (= (name t) local-name)))

(defn- find-children [el local-name]
  (->> (:content el) (filter map?) (filter #(tag-matches? % local-name))))

(defn- find-child [el local-name] 
  (first (find-children el local-name)))

(defn- child-text [el local-name]
  (some-> (find-child el local-name) :content first str/trim))

(defn- entity-id [el] (get-in el [:attrs :id]))
(defn- entity-ref [el] (get-in el [:attrs :ref]))

;; ---------------------------------------------------------------------------
;; ServiceJourneyInterchange parsing
;; ---------------------------------------------------------------------------

(defn parse-interchange [interchange]
  {:ID             (entity-id interchange)
   :stay-seated    (= "true" (child-text interchange "StaySeated"))
   :guaranteed     (= "true" (child-text interchange "Guaranteed"))
   :from-point-ref (entity-ref (find-child interchange "FromPointRef"))
   :to-point-ref   (entity-ref (find-child interchange "ToPointRef"))
   :from-journey   (entity-ref (find-child interchange "FromJourneyRef"))
   :to-journey     (entity-ref (find-child interchange "ToJourneyRef"))})

#_(defn extract-interchanges [timetable-frame]
  (->> (find-child timetable-frame "journeyInterchanges")
       (find-children "ServiceJourneyInterchange")
       (map parse-interchange)))

(defn extract-interchanges [timetable-frame]
  (let [ji (find-child timetable-frame "journeyInterchanges")]
    (->> (find-children ji "ServiceJourneyInterchange")
         (map parse-interchange))))


;; ---------------------------------------------------------------------------
;; ServiceJourney parsing (simplified)
;; ---------------------------------------------------------------------------

(defn parse-passing-time [pt]
  {:id                (entity-id pt)
   :stop-point-ref    (entity-ref (find-child pt "StopPointInJourneyPatternRef"))
   :arrival-time      (child-text pt "ArrivalTime")
   :departure-time    (child-text pt "DepartureTime")
   :arrival-offset    (child-text pt "ArrivalDayOffset")
   :departure-offset  (child-text pt "DepartureDayOffset")})

(defn parse-service-journey [sj]
  (let [day-types-el (find-child sj "dayTypes")
        passing-times-el (find-child sj "passingTimes")]
    {:id                  (entity-id sj)
     :name                (child-text sj "Name")
     :private-code        (child-text sj "PrivateCode")
     :day-type-refs       (mapv entity-ref (find-children day-types-el "DayTypeRef"))
     :journey-pattern-ref (entity-ref (find-child sj "JourneyPatternRef"))
     :operator-ref        (entity-ref (find-child sj "OperatorRef"))
     :line-ref            (entity-ref (find-child sj "LineRef"))
     :passing-times       (mapv parse-passing-time (find-children passing-times-el "TimetabledPassingTime"))}))

(defn extract-service-journeys [timetable-frame]
  (let [vj (find-child timetable-frame "vehicleJourneys")]
    (->> (find-children vj "ServiceJourney")
         (map parse-service-journey))))

;; ---------------------------------------------------------------------------
;; Build interchange graph
;; ---------------------------------------------------------------------------

(defn build-journey-index
  "Index journeys by id for fast lookup"
  [journeys]
  (reduce (fn [m j] (assoc m (:id j) j)) {} journeys))

(defn build-interchange-graph
  "Build adjacency map: journey-id -> [connected-journey-ids]"
  [interchanges]
  (reduce
   (fn [g {:keys [from-journey to-journey stay-seated]}]
     (if stay-seated
       (update g from-journey (fnil conj []) to-journey)
       g))
   {}
   interchanges))

(defn find-journey-chain
  "Follow stay-seated connections from a starting journey"
  [graph start-journey-id]
  (loop [current start-journey-id
         chain [current]
         visited #{current}]
    (let [nexts (get graph current [])]
      (if-let [next-j (first (remove visited nexts))]
        (recur next-j (conj chain next-j) (conj visited next-j))
        chain))))

(defn explore-line-file
  "Parse a line file and return exploration data"
  [file-path]
  (println ">>> 1. called")
  (when-let [pub-del (parser/parse-xml-file file-path)]
    (println ">>> 2. pub-del:" (some? pub-del))
    (let [cf (first (find-children (find-child pub-del "dataObjects") "CompositeFrame"))
          _ (println ">>> 3. cf:" (some? cf))
          frames (find-child cf "frames")
          _ (println ">>> 4. frames:" (some? frames))
          tf (first (find-children frames "TimetableFrame"))
          _ (println ">>> 5. tf:" (some? tf))
          
          journeys (extract-service-journeys tf)
          _ (println ">>> 6. journeys:" (count journeys))
          interchanges (extract-interchanges tf)
          
          journey-idx (build-journey-index journeys)
          graph (build-interchange-graph interchanges)]
      
      {:journey-count (count journeys)
       :interchange-count (count interchanges)
       :stay-seated-count (count (filter :stay-seated interchanges))
       :journeys journey-idx
       :interchanges interchanges
       :graph graph})))

;; ---------------------------------------------------------------------------
;; Exploration entry point
;; ---------------------------------------------------------------------------

(defn extract-journey-patterns [service-frame]
  (->> (find-child service-frame "journeyPatterns")
       (find-children "JourneyPattern")
       (map (fn [jp]
              {:id (entity-id jp)
               :stops (->> (find-child jp "pointsInSequence")
                           (find-children "StopPointInJourneyPattern")
                           (map (fn [spijp]
                                  {:id (entity-id spijp)
                                   :order (get-in spijp [:attrs :order])
                                   :stop-ref (entity-ref (find-child spijp "ScheduledStopPointRef"))
                                   :for-boarding (child-text spijp "ForBoarding")
                                   :for-alighting (child-text spijp "ForAlighting")})))}))
       (reduce (fn [m jp] (assoc m (:id jp) jp)) {})))


