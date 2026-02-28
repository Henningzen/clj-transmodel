;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/parser/core.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025 - 2026
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.parser.core
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.tools.logging :as log])
  (:import (java.io FileNotFoundException IOException)
           (javax.xml.stream XMLStreamException)))
;;
;;   Transmodel standards XML basic parser
;;   ------------------------------------
;;
;;   The parse-xml method operate on a Stream, either File or Reader.
;;
;;   authors:   Henning Jansen - henning.jansen@jansenh.no;
;;   since:     0.1.0                    2025-08-07
;;   version:   0.2.1
;;   ---------------------------------------------------------------------------
;;
;;       + TODO: Test the parse-xml for large files, streams closing.
;;               Do we need slurp here?
;;       + TODO: file->input-stream needs testing for close stream bevaviour.
;;

(defn- parse-xml
  "Parse XML from an input stream and return the parsed structure as Clojure
   data structures. Takes an input-stream as input.
   NOTE: This function does not close the stream - the caller is
   responsible for that."
  [input-stream]
  ;; Read the stream fully and parse as a string to avoid stream closed issues
  (xml/parse-str (slurp input-stream)))

(defn parse-xml-file
  "Parses an XML file using clojure.data.xml, handling errors gracefully.
   Takes the file path (string) as input.
   Returns the fully realized parsed XML structure as Clojure data on success.
   Returns nil and logs an error message if the file is not found,
   cannot be read, or contains malformed XML."
  [file-path]
  (try
    (with-open [input-stream (io/input-stream file-path)]
      (parse-xml input-stream))
    (catch FileNotFoundException _
      (log/warnf "XML file not found at path: %s" file-path)
      nil)
    (catch IOException e
      (log/errorf e "IOException reading XML file: %s" file-path)
      nil)
    (catch XMLStreamException e
      (log/errorf e "XML parsing error in file: %s" file-path)
      nil)
    (catch Exception e
      (log/errorf e "Unexpected error parsing XML file: %s" file-path)
      nil)))

(defn parse-xml-zip-file
  "Parse an XML file from within a zip file, and return the parsed structure
   as Clojure data structures. Takes a path to the zip file, relative to
   root, and the name of the XML file within the zip."
  [zip-path file-name]
  (try
    (with-open [zip (java.util.zip.ZipFile. (java.io.File. zip-path))]
      (when-let [entry (.getEntry zip file-name)]
        (with-open [input-stream (.getInputStream zip entry)]
          (parse-xml input-stream))))
    (catch java.io.FileNotFoundException _
      (log/warnf "ZIP file not found at path: %s" zip-path)
      nil)
    (catch java.nio.file.NoSuchFileException _
      (log/warnf "XML file not found: %s" file-name)
      nil)
    (catch java.util.zip.ZipException _
      (log/warnf "ZIP file exception: %s" zip-path)
      nil)))

(defn peek-xml
  "Peek the top content of a XML file. Function takes a clojure data structure
   representing the XML string from clojure.data.xml and returns an overview
   in a map."
  [parsed-xml]
  (when parsed-xml
    {:tag     (:tag parsed-xml)
     :attrs   (:attrs parsed-xml)
     :content (:content parsed-xml)}))
