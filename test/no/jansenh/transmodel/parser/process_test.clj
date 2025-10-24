(ns no.jansenh.transmodel.parser.process-test
  (:require [clojure.data.zip.xml :as zip-xml]
            [clojure.test :refer [deftest is testing]]
            [no.jansenh.transmodel.parser.core :as parser]
            [no.jansenh.transmodel.parser.process :as sut]))

(def network-timetable-version "1.15:NO-NeTEx-networktimetable:1.5")

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
      (zip-xml/xml1-> (sut/publication-delivery->zip xml-data)
                      sut/netex:PublicationTimestamp
                      zip-xml/text))))
