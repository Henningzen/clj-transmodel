;;-----------------------------------------------------------------------------
;; File: src/no/jansenh/transmodel/siri/service_delivery.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;-----------------------------------------------------------------------------

(ns no.jansenh.transmodel.siri.service-delivery
  (:require [no.jansenh.transmodel.siri.situation-exchange-delivery :as sx]
    [no.jansenh.transmodel.parser.core :as parser]))
;;
;;   SIRI Service Delivery
;;   ---------------------
;;
;;   authors: Henning Jansen - henning.jansen@jansenh.no;
;;   version: 0.1.0
;;   since: 0.1.0 2025-08-18
;;

(def siri-kw-ns "xmlns.http%3A%2F%2Fwww.siri.org.uk%2Fsiri") ;; http://www.siri.org.uk/siri

(def siri:Siri (keyword siri-kw-ns "Siri"))
(def siri:ServiceDelivery (keyword siri-kw-ns "ServiceDelivery"))
(def siri:ResponseTimestamp (keyword siri-kw-ns "ResponseTimestamp"))
(def siri:ProducerRef (keyword siri-kw-ns "ProducerRef"))
(def siri:EstimatedTimetableDelivery (keyword siri-kw-ns "EstimatedTimetableDelivery"))
(def siri:SituationExchangeDelivery (keyword siri-kw-ns "SituationExchangeDelivery"))

(defn parse-service-delivery [xml-data]
  (when (= (:tag xml-data) siri:Siri)
    (let [version (->> (:attrs xml-data) :version)

          content (:content xml-data)

          service-delivery (->> content
                                (filter #(= (:tag %) siri:ServiceDelivery))
                                first)

          response-timestamp (->> (:content service-delivery)
                                     (filter #(= (:tag %) siri:ResponseTimestamp))
                                     first
                                     :content
                                     (apply str))

          producer-ref (->> (:content service-delivery)
                           (filter #(= (:tag %) siri:ProducerRef))
                           first
                           :content
                           (apply str))

          situation-exchange-delivery  (sx/parse-situation-exchange-delivery (->> (:content service-delivery)
                                                                                  (filter #(= (:tag %) siri:SituationExchangeDelivery))
                                                                                  first))

          estimated-timetable-delivery  (->> (:content service-delivery)
                                             (filter #(= (:tag %) siri:EstimatedTimetableDelivery))
                                             first
                                             :content
                                             (apply str))]

      {:version version
       :response-timestamp response-timestamp
       :producer-ref producer-ref
       :situation-exchange-delivery situation-exchange-delivery
       :estimated-timetable-delivery estimated-timetable-delivery})))


(comment

  (def file "resources/siri/sx/20250818T010250083687.xml")

  (defn test-siri-data
    "comment"
    []
    (let [file-path file
          xml-data (parser/parse-xml-file file-path)]
      (when xml-data
        xml-data)))

  (parse-service-delivery (parser/parse-xml-file file))
  ;;--->
  )


