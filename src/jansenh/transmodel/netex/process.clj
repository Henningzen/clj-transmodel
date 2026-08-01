;;; jansenh/transmodel/netex/process.clj --- NeTEx processing
;;
;;   Copyright (c) Henning Jansen2025 - 2026
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 2.0 (https://opensource.org/license/epl-2-0)
;;   which can be found in the file LICENSE at the root of this distribution.s
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license. You must not remove this notice, or any other,
;;   from this software.
;;
;; Author:  Henning Jansen - henning.jansen@jansenh.no
;; Date:    September 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0
;;
;; SPDX-License-Identifier: EPL-2.0

(ns jansenh.transmodel.netex.process
  ^{:doc  "NeTEx processing, PublicationDelivery"
    :author "Henning Jansen"
    :added  "0.2.3"
    :license {:name "Eclipse Public License"
              :url "https://opensource.org/license/epl-2-0"}}
  (:require [jansenh.transmodel.parser.xml :as x]))

(defn process-publication-delivery
  "Returns the clojure map represent outermost PublicationDelivery element of a
  NeTEx XML document."
  [xml-data]
  (when (= (x/root-tag xml-data) "PublicationDelivery")
    {:version               (-> xml-data x/entity-version)
     :publication-timestamp (-> xml-data (x/child-text "PublicationTimestamp"))
     :description           (-> xml-data (x/child-text "Description"))
     :participant-ref       (-> xml-data (x/child-text "ParticipantRef"))}))

