;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/netex/process.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025 - 26
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.netex.process
  (:require [jansenh.transmodel.parser.utilities :as c]))
;;
;;   NeETx processor
;;   ---------------
;;
;;   TODO: Explore https://clojure.github.io/data.zip/#clojure.data.zip.xml
;;         for behavioral correctness.
;;   TODO: Make namespace lookups reusable and usable in general.
;;   TODO: Find appropriate name for process.clj
;;
;;   authors:   Henning Jansen - henning.jansen@jansenh.no;
;;   since:     0.1.0                    2025-08-07
;;   version:   0.1.0
;; -----------------------------------------------------------------------------
;;

(def netex-kw-ns "xmlns.http%3A%2F%2Fwww.netex.org.uk%2Fnetex")
(def netex:PublicationDelivery (keyword netex-kw-ns "PublicationDelivery"))
(def netex:PublicationTimestamp (keyword netex-kw-ns "PublicationTimestamp"))
(def netex:Description (keyword netex-kw-ns "Description"))
(def netex:ParticipantRef (keyword netex-kw-ns "ParticipantRef"))

(defn process-publication-delivery
  "Document me"
  [xml-data]
  (when (= (:tag xml-data) netex:PublicationDelivery)
    {:version               (-> xml-data
                                (c/attr :version))
     :publication-timestamp (-> xml-data
                                (c/find-child netex:PublicationTimestamp)
                                (c/text-content))
     :description           (-> xml-data
                                (c/find-child netex:Description)
                                (c/text-content))
     :participant-ref       (-> xml-data
                                (c/find-child netex:ParticipantRef)
                                (c/text-content))}))
