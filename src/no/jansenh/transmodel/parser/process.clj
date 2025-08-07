;;-----------------------------------------------------------------------------
;; File: src/no/jansenh/transmodel/parser/process.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns no.jansenh.transmodel.parser.process
  (:require [no.jansenh.transmodel.parser.core :as parser]
            [clojure.tools.logging :as log]
            [clojure.data.xml :as xml]
            [clojure.zip :as zip]
            [clojure.data.zip.xml :as zip-xml]))
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
;; ------------------------------------+----------------------------------------
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

      {:version version
       :publication-timestamp publication-timestamp
       :description description})))


(defn publication-delivery->zip [xml-data]
  (when (= (:tag xml-data) netex:PublicationDelivery)
    (zip/xml-zip xml-data)))


(defn data-objects->zip [xml-data]
  (when (= (:tag xml-data) netex:PublicationDelivery)
    (println "Found PublicationDelivery")
    (let [data-objects (->> (:content xml-data)
                            (filter #(= (:tag %) netex:dataObjects))
                            first
                            :content)]
      (zip/xml-zip data-objects))))


(comment


  (defn test-xml-data [] (let [file-path "resources/testdata/292.xml"
                                         xml-data (parser/parse-xml-file file-path)]
                                     (when xml-data
                                       xml-data)))

  (defn test-process-publication-delivery [] (let [file-path "resources/testdata/292.xml"
                                                   xml-data (parser/parse-xml-file file-path)]
                                               (when xml-data
                                                 (let [xml-data (process-publication-delivery xml-data)]
                                                   xml-data))))

  (defn test-publication-delivery->zip [] (let [file-path "resources/testdata/292.xml"
                                                xml-data (parser/parse-xml-file file-path)]
                                            (when xml-data
                                              (let [zipper (publication-delivery->zip xml-data)]
                                                (zip-xml/xml1-> zipper netex:PublicationTimestamp zip-xml/text)))))

  (defn test-data-objects-> [] (let [file-path "resources/testdata/292.xml"
                                       xml-data (parser/parse-xml-file file-path)]
                                   (when xml-data
                                     (data-objects->zip xml-data))))
  

  (test-xml-data)
  (test-process-publication-delivery)
  (test-publication-delivery->zip)
  (test-data-objects->)

  ;; --->
)
