;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/parser/xml.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2026
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.parser.xml
  "XML navigation utility functions for parsed clojure.data.xml structures."
  (:require [clojure.string :as str]))

;;
;;   Transmodel standards XML element navigation
;;   --------------------------------------------
;;
;;   Utility functions for traversing parsed XML trees from clojure.data.xml.
;;
;;   NOTE:
;;   All matching uses local tag names via (name tag), which strips the XML
;;   namespace. This is safe for NeTEx documents where all elements share a
;;   single namespace (http://www.netex.org.uk/netex).
;;
;;   NOTE:
;;   The definition of the functions are strictly tied to clojure.data.xml
;;   versioned around 0.2.0-alphaXXX at the time of writing (March 2026)
;;   The namespace is most likely compatible with stable version 0.0.8
;;   Most likely, as is most likely pregnant, or most likely nearby or most
;;   likely most likely.
;;
;;   Relationship to parser.core:
;;   core.clj  — reads XML from disk/zip into clojure.data.xml structures
;;   xml.clj   — navigates within those structures (find, filter, extract)
;;
;;   authors:   Henning Jansen - henning.jansen@jansenh.no;
;;   since:     0.2.1   2026-03-01
;;   version:   0.2.2   2026-03-01
;;   ---------------------------------------------------------------------------

(defn find-children
  "Find all direct child elements matching a local tag name"
  [elem local-name]
  (when elem
    (->> (:content elem)
         (filter map?)
         (filter #(= (some-> (:tag %) name) local-name)))))

(defn find-child
  "Find first direct child element matching a local tag name"
  [elem local-name]
  (first (find-children elem local-name)))

(defn child-text
  "Get text content of first child element matching tag name"
  [elem local-name]
  (some-> (find-child elem local-name)
          :content
          first
          str
          str/trim))

(defn entity-id      [elem] (get-in elem [:attrs :id]))
(defn entity-ref     [elem] (get-in elem [:attrs :ref]))
(defn entity-version [elem] (get-in elem [:attrs :version]))
(defn entity-order   [elem] (some-> (get-in elem [:attrs :order]) parse-long))

(defn find-all-deep
  "Recursively find all elements matching a local tag name throughout the tree."
  [element local-name]
  (when (map? element)
    (let [matching (if (= (some-> (:tag element) name) local-name)
                     [element]
                     [])
          children-matches (when (:content element)
                             (mapcat #(find-all-deep % local-name)
                                     (filter map? (:content element))))]
      (concat matching children-matches))))
