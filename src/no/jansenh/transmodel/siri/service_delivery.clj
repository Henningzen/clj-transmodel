;;-----------------------------------------------------------------------------
;; File: src/no/jansenh/transmodel/siri/service_delivery.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;-----------------------------------------------------------------------------

(ns no.jansenh.transmodel.siri.service-delivery
  (:require [no.jansenh.transmodel.parser.utilities :as u]))
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


(defn- service-delivery-type
  "Determines the type of service delivery present in the content.
   
   Returns:  the non-nil value as a string, or nil if both are nil.
   Throws:   an exception if both tags are present.
  "
  [content]
  (let [sx (u/tag-name content siri:SituationExchangeDelivery)
        et nil]
    (cond
      (and sx et) (throw (ex-info "Multiple service delivery tags found" {:content content}))
      sx sx
      et et
      :else nil)))


(defn parse-service-delivery 
  "TBA

   Returns: 
  "
  [xml-data]
  (when (= (:tag xml-data) siri:Siri)
    (let [version (:version (:attrs xml-data))
          service-delivery-elem (->> (:content xml-data)
                                     (filter #(= (:tag %) siri:ServiceDelivery))
                                     first)
          service-delivery-content (:content service-delivery-elem)]
      {:version               version
       :response-timestamp    (u/tag-content-datetime service-delivery-content siri:ResponseTimestamp)
       :producer-ref          (u/tag-text service-delivery-content siri:ProducerRef)       
       :service_delivery-type (service-delivery-type service-delivery-content)})))
