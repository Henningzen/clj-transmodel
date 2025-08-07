(ns no.jansenh.transmodel.parser.process-test
  (:require [no.jansenh.transmodel.parser.process :as sut]
            [no.jansenh.transmodel.parser.core :as parser]
            [clojure.data.zip.xml :as zip-xml]
            [clojure.test :refer [deftest is testing]]))

(def network-timetable-version "1.15:NO-NeTEx-networktimetable:1.5")

(def publication-timestamp  "2024-12-02T23:45:43.519")

(deftest test-process

  (testing "We expect explicit results when testing the 292.xml NeTEx
   file in the resources directory."
    (let [file-path "resources/testdata/292.xml"
          xml-data (parser/parse-xml-file file-path)]
      (when xml-data
        (let [xml-data (sut/process-publication-delivery xml-data)]
          (is (= network-timetable-version (:version xml-data)))))))

  (testing "We expect explicit results when testing the 292.xml NeTEx
   file in the resources directory."
    (let [file-path "resources/testdata/292.xml"
          xml-data (parser/parse-xml-file file-path)]
      (let [zipper (sut/publication-delivery->zip xml-data)]
        (zip-xml/xml1-> zipper sut/netex:PublicationTimestamp zip-xml/text)))))
