;;; dev/usr.clj --- User namespace for default REPL env

;   Copyright (c) Henning Jansen2025 - 2026
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 2.0 (https://opensource.org/license/epl-2-0)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license. You must not remove this notice, or any other,
;   from this software.
;
;; Author:  Henning Jansen - henning.jansen@jansenh.no
;; Date:    May 2026
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0
;;-----------------------------------------------------------------------------

(ns user
  ^{:doc    "User namespace for default REPL env"
    :author "Henning Jansen"
    :added  "0.2.3"
    :license {:name "Eclipse Public License"
              :url "https://opensource.org/license/epl-2-0"}}
  (:require
   [jansenh.transmodel.core :as core]
   [jansenh.transmodel.utilities.data-retriever :refer [download-netex-zip]]
   [clojure.repl :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.tools.namespace.repl :refer [refresh refresh-all]]))

;; Optional: Auto-refresh on file changes
;; (add-hook! :repl/after-load refresh)

(def data-store "/home/jansenh/data/netex")


(comment
 (download-netex-zip :SOF (str data-store "/raw/"))

 ;; --->
 )
