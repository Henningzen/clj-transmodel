;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/parser/process.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.parser.process
  (:require [jansenh.transmodel.parser.core :as parser]))
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
(def netex:dataObjects (keyword netex-kw-ns "dataObjects"))
(def netex:CompositeFrame (keyword netex-kw-ns "CompositeFrame"))

(defn process-publication-delivery [xml-data]
  (when (= (:tag xml-data) netex:PublicationDelivery)
    (let [version (->> (:attrs xml-data)
                       :version)

          content (:content xml-data)

          publication-timestamp (->> content
                                     (filter #(= (:tag %) netex:PublicationTimestamp))
                                     first
                                     :content
                                     (apply str))

          description (->> content
                           (filter #(= (:tag %) netex:Description))
                           first
                           :content
                           (apply str))]

      {:version               version
       :publication-timestamp publication-timestamp
       :description           description})))
