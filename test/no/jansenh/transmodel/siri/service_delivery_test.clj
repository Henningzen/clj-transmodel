(ns no.jansenh.transmodel.siri.service-delivery-test
  (:require [clojure.test :refer :all]
            [no.jansenh.transmodel.parser.core :as p]
            [no.jansenh.transmodel.siri.service-delivery :as sut])
  (:import (java.time Instant)))
;; 
;; 
;;
;;
;;
;; NOTE: this test is tied to Siri version 2.1
;;
;; TODO: Create a test-utils with the ReEx for DateTime Instant
;;

(def file "/home/jansenh/dev/clj-transmodel/resources/siri-sx-small.xml")
(def siri-version "2.1")

(deftest test-service-delivery-version
  " We are testing for Siri version 2.1
  "
  (let [service-delivery (sut/parse-service-delivery (p/parse-xml-file file))
        siri-version siri-version]
    (is (= siri-version (:version service-delivery)))))


(deftest test-service-delivery-response-timestamp
  "We expect that a service-delivery has a response-timestamp.
   The parse-service-delivery creates a proper Java Instant of this string.
  "
  (let [service-delivery (sut/parse-service-delivery (p/parse-xml-file file))]
    (is (instance? Instant (:response-timestamp service-delivery)))
    (is (re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+Z"
                    (str (:response-timestamp service-delivery))))))
