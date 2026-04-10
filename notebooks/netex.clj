(ns notebooks.netex
  (:require [jansenh.transmodel.parser.core :as parser]
            [jansenh.transmodel.netex.calendar :as cal]
            [jansenh.transmodel.parser.utilities :as utils]
            [scicloj.clay.v2.api :as clay]
            [scicloj.clay.v2.main]
            [scicloj.kindly.v4.kind :as kind]
            [tablecloth.api :as tc]
            [clojure.string :as str]))


;;  NeTEx explore notebook
;;  ======================
;;
;;    Notebook for NeTEx dataset exploration.
;;
;;    The implementation is based on Clojure with Noj, Clay, TableCloth et al.
;;    Visualization tools in use are from the JavaScript sphere of tooling.
;;
;;    The api refered in the :require section is a wrapper of a NeTEx parsing
;;    library https://github.com/Henningzen/clj-transmodel
;;
;;    The library is available under EPL 2.0 License but not yet made public.
;;
;; -----------------------------------------------------------------------------
;;  Henning Jansen 2025  Copyright © henning.jansen@jansenh.no
;;  Distributed under the Eclipse Public License version 2.0 as
;;  described in the README file under the root of this project.


(def shared-data-file "/home/jansenh/data/netex/KOL/_KOL_shared_data.xml")
(def line-data-file
  "/home/jansenh/data/netex/KOL/KOL_KOL-Line-8_5986_1025_Fogn---Judaberg---Helgoy.xml")

(def shared-data (parser/parse-xml-file shared-data-file))
(def line-data (parser/parse-xml-file line-data-file))

(def date-range (cal/weeks-ahead 1))


(def calendar-index (cal/build-calendar-index shared-data))
(def calendar-stats (:stats calendar-index))

(keys calendar-index)
(count (:operating-periods calendar-index))
(count (:day-types calendar-index))

(-> date-range
    (update :from utils/local-date->str)
    (update :to   utils/local-date->str)
    (tc/dataset        {:dataset-name "Date range"})
    (tc/rename-columns {:from "From" :to "To"}))

(-> calendar-stats
    (tc/dataset        {:dataset-name "Calendar statistics"})
    (tc/rename-columns {:day-type-count "day-type count" :period-count "period count" :assignement-count "assignement count"}))

;; -----------------------------------------------------------------------------

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
