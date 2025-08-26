;;-----------------------------------------------------------------------------
;; File: src/no/jansenh/transmodel/siri/service_delivery.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;-----------------------------------------------------------------------------

(ns no.jansenh.transmodel.siri.service-delivery
  (:require [no.jansenh.transmodel.siri.situation-exchange-delivery :as sx]
            [no.jansenh.transmodel.parser.utilities :as utils]
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
          service-delivery (utils/get-xml-tag content siri:ServiceDelivery)]
      {:version version
       :response-timestamp (utils/get-xml-tag service-delivery siri:ResponseTimestamp)
       :producer-ref (utils/get-xml-tag service-delivery siri:ProducerRef)
       :situation-exchange-delivery (utils/get-xml-tag service-delivery siri:SituationExchangeDelivery)
       :estimated-timetable-delivery (utils/get-xml-tag service-delivery siri:EstimatedTimetableDelivery)})))


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


