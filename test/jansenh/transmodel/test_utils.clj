;;-----------------------------------------------------------------------------
;; File: test/jansenh/transmodel/test_utils.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.test-utils)


;; NeETx testing utilities
;;
;; authors: Henning Jansen - henning.jansen@norled.no
;; version: 0.2.0 2025-12-19
;; since: 0.2.0 2025-12-19
;; -----------------------------------------------------------------------------

(defn valid-date-time?
  "Check if timestamp is valid ISO-8601 local date time.

   This is what we must anticipate for validity within the NeTEx and SIRI
   standards echo-system. Go see the tests!

   Returns: truthy on valid timestamp
  "
  [timestamp]
   ;; This function will probably be duplicated into src/ at some point. Let's
   ;; keep that duplication if it happens; I wanted to tie this test for validity
   ;; inside scope of tests for obvious and non-obvious reasons:
   ;; - verbosity (if needed)
   ;; - evolving edge cases
  (if (nil? timestamp)
    nil
    (re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}.\d{3}"
                timestamp)))



