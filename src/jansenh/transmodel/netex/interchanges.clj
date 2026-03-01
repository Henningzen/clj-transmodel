;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/netex/interchanges.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025 - 2026
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.netex.interchanges
  (:require [jansenh.transmodel.netex.registry :as registry]))

(defn last-passing-time
  "Get the last (final arrival) passing time for a journey"
  [journey]
  (last (:passing-times journey)))

(defn first-passing-time
  "Get the first (initial departure) passing time for a journey"
  [journey]
  (first (:passing-times journey)))

(defn visualize-interchange [interchange]
  (let [{:keys [from-journey to-journey from-point-ref to-point-ref
                stay-seated guaranteed]} interchange

        from-sj (registry/service-journey from-journey)
        to-sj (registry/service-journey to-journey)
        from-stop (registry/stop-point from-point-ref)
        to-stop (registry/stop-point to-point-ref)
        from-last-time (last-passing-time from-sj)
        to-first-time (first-passing-time to-sj)]

    (if-not (and from-sj to-sj from-stop to-stop)
      (println (format "❌ Incomplete interchange %s - missing data"
                      (:ID interchange)))
      (do
        ;; Outbound journey info
        (println (format "\n📍 FROM: %s (%s)"
                         (:name from-sj)
                         (:ID from-sj)))
        (println (format "   arrives %s at %s"
                         (:arrival-time from-last-time)
                         (:name from-stop)))

        ;; Connection indicator
        (println "   │")
        (if stay-seated
          (println "   ▼ 🚢 STAY SEATED - passenger remains on vessel")
          (println "   ▼ 🚶 INTERCHANGE - passenger changes vessel"))

        (if guaranteed
          (println "   │ ✓ GUARANTEED connection")
          (println "   │ ⚠ NON-GUARANTEED connection"))

        (println "   │")

        ;; Inbound journey info
        (println (format "📍 TO: %s (%s)"
                         (:name to-sj)
                         (:ID to-sj)))
        (println (format "   departs %s from %s"
                         (:departure-time to-first-time)
                         (:name to-stop)))))))

(defn visualize-journey-interchanges [journey-id]
  "Show all interchanges involving a specific journey"
  (let [journey (registry/service-journey journey-id)]
    (if-not journey
      (println (format "❌ Journey not found: %s" journey-id))
      (let [all-ic (registry/all-interchanges)
            outbound (filter #(= (:from-journey %) journey-id) all-ic)
            inbound (filter #(= (:to-journey %) journey-id) all-ic)]

        (println (format "\n📅 INTERCHANGES FOR: %s" (:name journey)))
        (println (format "   ID: %s" journey-id))

        (if (empty? outbound)
          (println "\n   🔴 No outbound connections")
          (do
            (println (format "\n   🟢 OUTBOUND CONNECTIONS (%d):" (count outbound)))
            (doseq [ic outbound]
              (visualize-interchange ic))))

        (if (empty? inbound)
          (println "\n   🔴 No inbound connections")
          (do
            (println (format "\n   🟡 INBOUND CONNECTIONS (%d):" (count inbound)))
            (doseq [ic inbound]
              (visualize-interchange ic))))))))

(defn visualize-all-interchanges
  "Show all interchanges in the system"
  []
  (let [all-ic (registry/all-interchanges)]
    (println (format "\n🔗 TOTAL INTERCHANGES: %d\n" (count all-ic)))
    (doseq [ic all-ic]
      (visualize-interchange ic))))

(defn interchange-stats
  "Statistics about interchanges"
  []
  (let [all-ic (registry/all-interchanges)
        stay-seated (count (filter :stay-seated all-ic))
        guaranteed (count (filter :guaranteed all-ic))]
    (println (format "\n📊 INTERCHANGE STATISTICS:"))
    (println (format "  Total: %d" (count all-ic)))
    (println (format "  Stay Seated: %d" stay-seated))
    (println (format "  Guaranteed: %d" guaranteed))
    (println (format "  StaySeated + Guaranteed: %d"
                     (count (filter #(and (:stay-seated %) (:guaranteed %)) all-ic))))))

(defn roundtrip-chains
  "Find connected journey chains (A→B→C patterns)"
  []
  (let [all-ic (registry/all-interchanges)
        stay-seated-only (filter :stay-seated all-ic)]
    (->> stay-seated-only
         (group-by :from-journey)
         (map (fn [[from-id connections]]
                {:from-journey from-id
                 :connections (map :to-journey connections)
                 :chain-length (count connections)})))))
