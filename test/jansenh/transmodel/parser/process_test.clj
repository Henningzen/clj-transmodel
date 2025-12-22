(ns jansenh.transmodel.parser.process-test
  (:require [clojure.test :refer [deftest is testing]]
            [jansenh.transmodel.parser.core :as parser]
            [jansenh.transmodel.parser.process :as sut]
            [jansenh.transmodel.test-utils :as u]))

(def network-timetable-version "1.15:NO-NeTEx-networktimetable:1.5")

(deftest test-process
  (testing "We expect explicit results when testing the 292.xml NeTEx
   file in the resources directory."
    (let [file-path "resources/testdata/292.xml"
          xml-data (parser/parse-xml-file file-path)]
      (when xml-data
        (let [publication-delivery (sut/process-publication-delivery xml-data)]
          (is (= network-timetable-version (:version publication-delivery)))
          (is (u/valid-date-time? (:publication-timestamp publication-delivery)))
          (is (string? (:description publication-delivery)))
          (is (string? (:participant-ref publication-delivery))))))))

