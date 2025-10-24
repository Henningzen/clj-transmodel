;;-----------------------------------------------------------------------------
;; File: src/no/jansenh/transmodel/siri/situation_exchange__delivery.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;-----------------------------------------------------------------------------

(ns no.jansenh.transmodel.siri.situation-exchange-delivery)
;;
;;   SIRI Situation Exchange Delivery
;;   --------------------------------
;;
;;   NOTE: This function is tied to XSD namespace http://www.siri.org.uk/siri
;;         in the SIRI standard.
;;
;;   authors: Henning Jansen - henning.jansen@jansenh.no;
;;   version: 0.1.0
;;   since: 0.1.0 2025-08-18
;;

(def siri-kw-ns "xmlns.http%3A%2F%2Fwww.siri.org.uk%2Fsiri")

(def siri:ResponseTimestamp (keyword siri-kw-ns "ResponseTimestamp"))
(def siri:SituationExchangeDelivery (keyword siri-kw-ns "SituationExchangeDelivery"))

(defn parse-situation-exchange-delivery
  "Tba "
  [sx-elem]
  (when (= (:tag sx-elem) siri:SituationExchangeDelivery)
    (let [response-timestamp (->> (:content sx-elem)
                                  (filter #(= (:tag %) siri:ResponseTimestamp))
                                  first
                                  :content
                                  (apply str))]
      {:response-timestamp response-timestamp})))




