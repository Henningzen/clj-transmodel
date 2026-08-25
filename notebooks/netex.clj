(ns notebooks.netex
  (:require [jansenh.transmodel.parser.core :as parser]
            [jansenh.transmodel.netex.calendar :as cal]
            [jansenh.transmodel.siri.utilities :as utils]
            [jansenh.transmodel.api :as api]
            [user :as user]
            [scicloj.clay.v2.api :as clay]
            [tablecloth.api :as tc]))


;;  NeTEx explore notebook
;;  ======================
;;
;;  Notebook for NeTEx dataset exploration
;;  --------------------------------------
;;
;;  The implementation is based on Clojure with Noj, Clay, TableCloth et al.
;;  Visualization tools in use are from the JavaScript sphere of tooling.
;;
;;  The `api` refered to in the :require section is a wrapper of a NeTEx parsing
;;  library https://github.com/Henningzen/clj-transmodel
;;
;; -----------------------------------------------------------------------------
;;  Copyright © Henning Jansen henning.jansen@jansenh.no 2025-2026  
;;  Distributed under the Eclipse Public License version 2.0 as
;;  described in the README file under the root of this project.
;;
;; -----------------------------------------------------------------------------
;;

(defn remove-nil-vals
  "It is what it is - remove nil's from maps"
  [m]
  (into {} (remove (comp nil? val)) m))

(def shared-data (parser/parse-xml-file "/home/jansenh/data/netex/KOL/_KOL_shared_data.xml"))

(def calendar-index (cal/build-calendar-index shared-data))


(user/dev) ;; TODO: Implement data loading facilities in either dev/user or a
           ;;       notebooks/ namespace

;; #### **Prepared date-range**
(-> (cal/weeks-ahead 1)
    (update :from utils/local-date->str)
    (update :to   utils/local-date->str)
    (tc/dataset {:dataset-name "Date range"})
    (tc/rename-columns {:from "From" :to "To"}))


;; #### **Calendar index in shared dataset**
(-> (:stats calendar-index)
    (tc/dataset {:dataset-name "Calendar statistics"})
    (tc/replace-missing :all :value "---")
    (tc/rename-columns {:day-type-count    "day-type count"
                        :period-count      "period count"
                        :assignement-count "assignement count"}))


;; #### **Operators, Public Transporation Agents**
(->  api/operators
     (tc/dataset {:dataset-name "PTA operators"})
     (tc/replace-missing :all :value "---")
     (tc/rename-columns {:id          "Id"
                         :name        "Name"
                         :short-name  "Short name"}))


;; #### **Stop-points total in shared dataset:**
;; 
(count api/all-stop-points)


;; #### **Stop-points peak 10**

(-> (->> api/all-stop-points
         vals
         (take 10))
    (tc/dataset {:dataset-name "StopPoints"})
    (tc/replace-missing :all :value "---")
    (tc/rename-columns {:id "Id"
                        :name "Name"
                        :short-name "Short name"}))


;; #### **Lines**
(->  api/lines
     (tc/dataset {:dataset-name "Lines"})
     (tc/replace-missing :all :value "---")
     (tc/dataset {:dataset-name "Line data"})
     (tc/rename-columns {:id                 "Id"
                         :version            "Version"
                         :name               "Name"
                         :public-code        "Public code"
                         :private-code       "Private code"
                         :operator-ref       "Operator ref"
                         :transport-mode     "Transport mode"
                         :transport-submode  "Transport submode"}))


(comment
  
  "Clay specifics
   --------------
   - rendering and behaviour"

  (clay/make! {:format [:html]
               :source-path "notebooks/netex.clj"
               :title "NeETx Transmodel"
               :browse true
               :show true
               :live-reload true
               :hide-ui-header false
               :hide-info-line true
               :hide-code true})
  
  ;;------------------------------------------------------------------>  comment
  ;;
  )


