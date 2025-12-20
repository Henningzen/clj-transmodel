;;-----------------------------------------------------------------------------
;; File: test/jansenh/transmodel/test_utilis_test.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.test-utils-test
  (:require [clojure.test :refer [deftest is testing]]
            [jansenh.transmodel.test-utils :refer [valid-date-time?]]))


(deftest test-valid-date-time?
  (testing "Valid ISO-8601 local datetime strings"
    ;; Valid cases
    (is (valid-date-time? "2025-11-24T00:35:56.095"))
    (is (valid-date-time? "2024-12-02T23:45:43.519"))
    (is (valid-date-time? "1999-01-01T00:00:00.000")))

  (testing "Invalid strings"
    ;; Missing milliseconds
    (is (not (valid-date-time? "2025-11-24T00:35:56")))
    ;; Wrong separator (space instead of T)
    (is (not (valid-date-time? "2025-11-24 00:35:56.095")))
    ;; Malformed
    (is (not (valid-date-time? "not-a-timestamp")))
    ;; Empty string
    (is (not (valid-date-time? "")))
    ;; Nil input
    (is (not (valid-date-time? nil))))

  (testing "Edge cases"
    ;; Extra characters
    (is (not (valid-date-time? "2025-11-24T00:35:56.095Z")))
    ;; Wrong order (e.g., time before date)
    (is (not (valid-date-time? "00:35:56.095T2025-11-24")))))
