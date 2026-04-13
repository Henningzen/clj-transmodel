(ns jansenh.transmodel.generator.timetable-test
  (:require [clojure.test :refer [deftest is testing]]
            [jansenh.transmodel.generator.timetable :as sut]
            [jansenh.transmodel.parser.core :as parser]))

(def resources-path "resources/testdata")
(def test-data-zip "test-data.zip")
(def shared-data-xml "shared_data.xml")
(def line-data "292.xml")

(defn- pathify
  "Pathify!"
  [path file]
  (str path "/" file))


(deftest test-parse-passing-time
  (let [data (parser/parse-xml-file (pathify resources-path line-data))]

    (testing "We should have some XML gdata  when we peek the resourses/testdata/292.xml file"
      (is (some? data) "XML data should not be nil"))))

(deftest test-parse-service-journeys
  (let [data (parser/parse-xml-file (pathify resources-path line-data))]

    (testing "We should have some ServiceJourneys from the test-data xml."
      (is (some? data) "XML data should not be nil"))
    
    (testing "We should have some ServiceJourneys from the test-data xml."
      (is (some? data) "XML data should not be nil"))))

(comment
(def netex-kw-ns "xmlns.http%3A%2F%2Fwww.netex.org.uk%2Fnetex")
(def netex:PublicationDelivery (keyword netex-kw-ns "PublicationDelivery"))
(def netex:PublicationTimestamp (keyword netex-kw-ns "PublicationTimestamp"))
(def netex:Description (keyword netex-kw-ns "Description"))
(def netex:ParticipantRef (keyword netex-kw-ns "ParticipantRef"))

(let [data (parser/parse-xml-file (pathify resources-path line-data))]
  (->> (:tag data)
       (netex:PublicationDelivery)
       )
  data)
  ;; --->
  )

