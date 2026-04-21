(ns netex
  (:require [jansenh.transmodel.parser.core :as parser]
            [jansenh.transmodel.netex.calendar :as cal]
            [jansenh.transmodel.parser.utilities :as utils]
            [jansenh.transmodel.api :as api]
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

(def shared-data (parser/parse-xml-file "/home/jansenh/data/netex/KOL/_KOL_shared_data.xml"))

(def calendar-index (cal/build-calendar-index shared-data))

(def lines api/lines)

(-> (cal/weeks-ahead 1)
    (update :from utils/local-date->str)
    (update :to   utils/local-date->str)
    (tc/dataset        {:dataset-name "Date range"})
    (tc/rename-columns {:from "From" :to "To"}))

;; -----------------------------------------------------------------------------

(-> (:stats calendar-index)
    (tc/dataset        {:dataset-name      "Calendar statistics"})
    (tc/rename-columns {:day-type-count    "day-type count"
                        :period-count      "period count"
                        :assignement-count "assignement count"}))

;; -----------------------------------------------------------------------------

(-> (first lines)
    (as-> m (into {} (remove (comp nil? val)) m)) ;; TODO!
    (tc/dataset        {:dataset-name   "Line data"})
    (tc/rename-columns {:id             "Id"
                        :version        "Version"
                        :name           "Name"
                        :public-code    "Public code"
                        :private-code   "Private code"
                        :operator-ref   "Operator ref"
                        :transport-mode "Transport mode"}))

















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


