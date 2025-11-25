;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/parser/utilities.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------


(ns jansenh.transmodel.parser.utilities
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
;; version: 0.1.0 2025-09-15
;; since: 0.1.0 2025-08-07
;; -----------------------------------------------------------------------------
;;


(defn- ctag
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


(defn tag-element
  "Get the entire element (tag, attrs, content) with the given tag.

   Returns: TBA
  "
  [content tag]
  (->> (ctag content tag)
       first))


(defn tag-name
  "Get just the local tag name of the first matching element.

   This is useful for getting the top level XML tag of a nested structure.

   Returns: tag name or nil if the tag is not present or the content is nil."
  [content tag]
  (when (and content (seq content))                         ;; We go via 'seq to ensure a safe landing
    (->> (ctag content tag)                                 ;; if nil.
         first
         :tag
         name)))                                            ; Use 'name' to extract just the local part of the keyword


(defn tag-attr
  "Get a specific attribute value from the first element with the given tag.

   Usage:
     (tag-attr content: MyElement :id)
     (tag-attr content: MyElement \"class\")

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


;; ----------------------------------------------------------------------------
;;
;; DateTime handling
;;
;; ----------------------------------------------------------------------------

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
