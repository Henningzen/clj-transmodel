(ns jansenh.transmodel.parser.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [jansenh.transmodel.parser.core :as sut]))

(def resources-path "resources/testdata")
(def test-data-zip "test-data.zip")
(def line-data "resources/testdata/292.xml")
(def shared-data-xml "shared_data.xml")

(def timetable-version "1.15:NO-NeTEx-networktimetable:1.5")

(defn pf
  "Pathify!"
  [path file]
  (str path "/" file))

;; -----------------------------------------------------------------------------
;; Test the file parser, with zip file and regular XML file.
;;
;; Look out for an expected error-message in the REPL and/or logs, as we poke
;; exceptions in these tests.
;;

(deftest test-parse-xml-file
  (let [data (sut/parse-xml-file line-data)]

    (testing "We should have some XMLdata  when we peek the resourses/testdata/292.xml file"
      (is (some? data) "XML data should not be nil")
      (is (= timetable-version (:version (:attrs (sut/peek-xml data))))))

    (testing "We should have some attribute, tag and content at the root level"
      (is (some? (:tag     (sut/peek-xml data))))
      (is (some? (:attrs   (sut/peek-xml data))))
      (is (some? (:content (sut/peek-xml data)))))))


(deftest test-parse-xml-zip-file
  (let [data (sut/parse-xml-zip-file (pf resources-path test-data-zip) shared-data-xml)]

    (testing "We should have some XML data"
      (is (= true (some? data)) "XML data should not be nil"))

    (testing "We should have some attribute, tag and content at the root level"
      (is (some? (:tag     (sut/peek-xml data))))
      (is (some? (:attrs   (sut/peek-xml data))))
      (is (some? (:content (sut/peek-xml data)))))

    (testing "When file is missing or not readable"
      (let [missing-xml (sut/parse-xml-zip-file (pf resources-path "non-existing.zip") shared-data-xml)
            wrong-entry-xml (sut/parse-xml-zip-file(pf resources-path test-data-zip)  "nonexistent.xml")]

        (testing "Missing zip file should return nil"
          (is (nil? missing-xml) "Missing zip file should return nil"))

        (testing "Missing entry in zip should return nil"
          (is (nil? wrong-entry-xml) "Missing entry should return nil"))))))
