;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/parser/utilities.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025 - 2026
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.parser.utilities
  (:require [clojure.string :as str])
  (:import (java.time Instant LocalDateTime OffsetDateTime ZoneOffset ZonedDateTime)
           (java.time.format DateTimeParseException)))

;; NeETx parser utilities
;;
;; The namespace handle XML acrobatics:
;;   - attributes on a tag
;;   - all attributes on a tag
;;   - tag element
;;   - tag text
;;
;; The namespace handles DateTime with not-yet-tested elegance:
;;   - date-time tag attribute
;;   - date-time on element
;;   - 
;; authors: Henning Jansen - henning.jansen@norled.no
;; version: 0.2.0 2026-03-01
;; since:   0.1.0 2025-08-07
;; -----------------------------------------------------------------------------


(def netex-ns "xmlns.http%3A%2F%2Fwww.netex.org.uk%2Fnetex")

(defn nkw [local-name] (keyword netex-ns local-name))



;;------------------------------------------------------------------------------
;; XML Parsing - core primitives
;;

(defn ctag
  [content tag]
  (->> content
       (filter #(= (:tag %) tag))))

(defn tag-content-seq
  "Get the content of the first element with the given tag.

   Returns: the natural seq from raw clojure.xml-data
  "
  [content tag]
  (->> (ctag content tag)
       first
       :content))

(defn tag-text
  "Get the content of the first element with the given tag.

   Returns: the text element of a given tag.
  "
  [content tag]
  (first (tag-content-seq content tag)))

(defn tag-name

  ;; TODO : this is implemented in siri/service-delivery.clj
  ;;        FIgure out what to do with the function, and if I
  ;;        can re-use it in NeTEx top level tag. 
  "Get just the local tag name of the first matching element.

   This is useful for getting the top level XML tag of a nested structure.

   Returns: tag name or nil if the tag is not present or the content is nil."
  [content tag]
  (when (and content (seq content)) ;; We go via 'seq to ensure a safe landing
    (->> (ctag content tag)         ;; if nil.
         first
         :tag
         name))) ; Use 'name' to extract just the local part of the keyword


;;------------------------------------------------------------------------------
;; Attribute extraction functions
;;

(defn attr
  "Get attribute value from a given tag"
  [element attr-name]
  (get (:attrs element) attr-name))

(defn tag-attr
  "Get a specific attribute value from the first element with the given tag.

   Usage:
     (tag-attr content MyElement :id)
     (tag-attr content MyElement \"class\")

   Returns: the attribute value or nil if tag or attribute not found.
  "
  [content tag attr-name]
  (->> (ctag content tag)
       first
       :attrs
       attr-name))

(defn tag-attrs
  "Get all attributes from the first element with the given tag.

   Usage:
     (tag-attrs content: MyElement)

   Returns: the entire attrs map or nil if tag not found.
  "
  [content tag]
  (->> (ctag content tag)
       first
       :attrs))

(defn element-id
  "Get the :id attribute from an element"
  [element]
  (get-in element [:attrs :id]))

(defn element-ref
  "Get the :ref attribute from an element (for references)"
  [element]
  (get-in element [:attrs :ref]))

;;-----------------------------------------------------------------------------
;; XML Navigation - finding and filtering elements
;;
;; NOTE: these are the functions being used in process.clj moving forward.
;; TODO: ADD TESTS ASAP!

(defn find-children
  "Find all direct children with a given tag name in element's content.
   Filters out string content (whitespace).
   Works with both namespaced and non-namespaced tags."
  [element tag]
  (when (and element (:content element))
    (->> (:content element)
         (filter map?)
         (filter #(= (:tag %) tag)))))

(defn find-child
  "Find the first direct child with a given tag.
   Works with both namespaced and non-namespaced tags."
  [element tag]
  (first (find-children element tag)))

(defn find-all-tags
  "Recursively find ALL elements with a given tag throughout the tree.
   Searches deeply through all descendants."
  [element tag]
  (when element
    (let [matching (if (= (:tag element) tag) [element] [])
          children-matches (when (:content element)
                             (mapcat #(find-all-tags % tag) (:content element)))]
      (concat matching children-matches))))

(defn text-content
  "Get text content of element"
  [element]
  (->> (:content element)
       (filter string?)
       (apply str)
       str/trim))

;;-----------------------------------------------------------------------------
;; Combined extraction - element + content

(defn get-text
  "Get text content from an element.
   Extracts the first string in :content."
  [element]
  (when element
    (first (filter string? (:content element)))))

(defn deep-find-text
  "Find a nested element and get its text content.
   Usage: (deep-find-text element 'TimetabledPassingTime' 'DepartureTime')
   Finds TimetabledPassingTime child, then DepartureTime child within it."
  [element tag1 tag2]
  (when-let [child1 (find-child element tag1)]
    (get-text (find-child child1 tag2))))

;;------------------------------------------------------------------------------
;; NeTEx/SIRI specific patterns

(defn attrs-defaults->map
  "Return the common Id, Version, Created attributes of any NeTEx/SIRI element"
  [elem]
  {:id      (-> elem (attr :id))
   :created (-> elem (attr :created))
   :version (-> elem (attr :version))})


;; -----------------------------------------------------------------------------
;;
;; DateTime handling
;;
;; -----------------------------------------------------------------------------

(defn parse-datetime
  "Parse XML datetime string to Java Instant using raw Java APIs.
   Handles common XML datetime formats (ISO-8601, etc.)"
  [datetime-str]
  (when (and datetime-str (seq datetime-str))
    (try
      ;; Try different common XML datetime formats
      (cond
        ;; ISO instant format: 2025-01-15T14:30:00.000Z
        (re-find #"Z$" datetime-str)
        (Instant/parse datetime-str)

        ;; Offset format: 2025-01-15T14:30:00+01:00
        (re-find #"[+-]\d{2}:\d{2}$" datetime-str)
        (.toInstant (OffsetDateTime/parse datetime-str))

        ;; Zoned format: 2025-01-15T14:30:00[Europe/Oslo]
        (re-find #"\[.+\]$" datetime-str)
        (.toInstant (ZonedDateTime/parse datetime-str))

        ;; Local datetime (assume UTC): 2025-01-15T14:30:00
        (re-find #"T\d{2}:\d{2}:\d{2}" datetime-str)
        (.toInstant (.atZone (LocalDateTime/parse datetime-str) ZoneOffset/UTC))

        ;; Fallback: try direct parsing
        :else
        (Instant/parse datetime-str))

      (catch DateTimeParseException e
        (println "Failed to parse datetime:" datetime-str "Error:" (.getMessage e))
        nil)
      (catch Exception e
        (println "Unexpected error parsing datetime:" datetime-str "Error:" (.getMessage e))
        nil))))

(defn tag-content-datetime
  "Get datetime content from a tag and parse it to Instant.
   Handles: <ResponseTimestamp>2025-01-15T14:30:00Z</ResponseTimestamp>"
  [content tag]
  (-> (tag-text content tag)
      parse-datetime))

(defn tag-attr-datetime
  "Get the datetime attribute from a tag and parse it to Instant.
   Handles: <DepartureTime timestamp='2025-01-15T14:30:00Z'>...</DepartureTime>"
  [content tag attr-name]
  (-> (tag-attr content tag attr-name)
      parse-datetime))

(defn datetime-from-tag
  "Universal datetime extractor that tries both content and attributes.
   Returns Java Instant or nil if parsing fails."
  ([content tag]
   (datetime-from-tag content tag nil))
  ([content tag attr-name]
   (cond
     ;; Try specific attributes
     (and attr-name (not= attr-name :both))
     (tag-attr-datetime content tag attr-name)

     ;; Try both content and common datetime attributes
     (= attr-name :both)
     (or (tag-content-datetime content tag)
         (tag-attr-datetime content tag :timestamp)
         (tag-attr-datetime content tag :time)
         (tag-attr-datetime content tag :dateTime))

     ;; Default: try content first, then common attributes as fallback
     :else
     (or (tag-content-datetime content tag)
         (tag-attr-datetime content tag :timestamp)
         (tag-attr-datetime content tag :time)))))


(comment
 
  (require '[jansenh.transmodel.parser.core :as parser])
  
  ;; ---------------------------------------------------------------------------
  ;; The current view of process function
  ;;

  (require '[jansenh.transmodel.parser.core :as parser])
  
  (let [file-path "resources/testdata/292.xml"
        xml-data (parser/parse-xml-file file-path)]
    (when xml-data
      {:publication-timestamp   (-> xml-data
                                    (find-child (nkw "PublicationTimestamp"))
                                    text-content)
       :participant-ref         (-> xml-data
                                    (find-child (nkw "ParticipantRef"))
                                    text-content)
       :description             (-> xml-data
                                    (find-child (nkw "Description"))
                                    text-content)
       
       :composite-frame {:attrs (-> (find-child xml-data (nkw "dataObjects"))
                                    (find-child (nkw "CompositeFrame"))
                                    (attrs-defaults->map))}}))

  (let [file-path "resources/testdata/292.xml"
        xml-data (parser/parse-xml-file file-path)]
    (when xml-data
      (:content xml-data)))



  ;; ---> 
  )
