(ns no.jansenh.transmodel.parser.core-test
  (:require [no.jansenh.transmodel.parser.core :as sut]
            [clojure.test :refer [deftest is testing]]))


(deftest test-parse-xml-zip-file
  (testing "We should have some XML when we peek the resourses/testdata/test_data.xml file"
    (let [shared-data-xml (sut/parse-xml-zip-file "resources/testdata/test-data.zip" "shared_data.xml")]
      (is (some? shared-data-xml) "XML should not be nil")
      #_(is (= "1.15:NO-NeTEx-networktimetable:1.5" (:version (:attrs (sut/peek-xml shared-data-xml)))))))

  (testing "When file is missing or not readable"
    (let [missing-xml (sut/parse-xml-zip-file "resources/testdata/nonexistent.zip" "shared_data.xml")
          wrong-entry-xml (sut/parse-xml-zip-file "resources/testdata/test-data.zip" "nonexistent.xml")]
      (testing "Missing zip file should return nil"
        (is (nil? missing-xml) "Missing zip file should return nil"))
      (testing "Missing entry in zip should return nil"
        (is (nil? wrong-entry-xml) "Missing entry should return nil")))))

(deftest test-parse-xml-file
  (testing "We should have some XML when we peek the resourses/testdata/292.xml file"
    (let [xml-file (sut/parse-xml-file "resources/testdata/292.xml")]
      (is (some? xml-file) "XML should not be nil")
      #_(is (= "1.15:NO-NeTEx-networktimetable:1.5" (:version (:attrs (sut/peek-xml shared-data-xml))))))))


(deftest test-zip-file-xml
  (let [shared-data-xml (sut/parse-xml-zip-file "resources/testdata/test-data.zip" "shared_data.xml")
        wrong-entry-xml (sut/parse-xml-zip-file "resources/testdata/test-data.zip" "nonexistent.xml")]
    #_(testing "We should have some XML"
      (is (= true (some? shared-data-xml)) "XML should not be nil"))
    (testing "We should have some attribute, tag and content at the root level"
      (is (not (nil? (some? (:attr (sut/peek-xml shared-data-xml) "attrs should not be nill")))))
      (is (some? (:tag (sut/peek-xml shared-data-xml) "tag should not be nill")))
      (is (= true (some? (:conten (sut/peek-xml shared-data-xml) "content should not be nill")))))
    (testing "When file is missing or not readable"
      (let [missing-xml (sut/parse-xml-zip-file "resources/testdata/nonexistent.zip" "shared_data.xml")]
        (testing "Missing zip file should return nil"
          (is (nil? missing-xml) "Missing zip file should return nil"))
        (testing "Missing entry in zip should return nil"
          (is (nil? wrong-entry-xml) "Missing entry should return nil"))))))
