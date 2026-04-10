;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/utilities/data_retriever.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.utilities.data-retriever
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Transmodel data retriever functions
;;
;; authors: Henning Jansen - henning.jansen@norled.no
;; version: 0.2.0 2025-12-19
;; since: 0.2.0 2025-12-19
;; -----------------------------------------------------------------------------


(def netex-zip-files {:ATB "https://storage.googleapis.com/marduk-production/outbound/netex/rb_atb-aggregated-netex.zip"
                      :KOL "https://storage.googleapis.com/marduk-production/outbound/netex/rb_kol-aggregated-netex.zip"
                      :MOR "https://storage.googleapis.com/marduk-production/outbound/netex/rb_mor-aggregated-netex.zip"
                      :NOR "https://storage.googleapis.com/marduk-production/outbound/netex/rb_nor-aggregated-netex.zip"
                      :OST "https://storage.googleapis.com/marduk-production/outbound/netex/rb_ost-aggregated-netex.zip"
                      :RUT "https://storage.googleapis.com/marduk-production/outbound/netex/rb_rut-aggregated-netex.zip"
                      :SKY "https://storage.googleapis.com/marduk-production/outbound/netex/rb_sky-aggregated-netex.zip"
                      :SOF "https://storage.googleapis.com/marduk-production/outbound/netex/rb_sof-aggregated-netex.zip"
                      :TRO "https://storage.googleapis.com/marduk-production/outbound/netex/rb_tro-aggregated-netex.zip"})

(defn download-netex-zip
  "Downloads a Netex zip file based on the provided PTA argument and saves it
  to the resources folder.
  PTA should be one of the keys in the netex-zip-files map."
  [pta]
  (if-let [url (get netex-zip-files pta)]
    (try
      (let [filename (str "resources/" (last (str/split url #"/")))
            file (io/file filename)]
        (with-open [in (io/input-stream url)
                    out (io/output-stream file)]
          (io/copy in out))
        (println (str "Downloaded " filename " successfully."))
        filename)
      (catch Exception e
        (println (str "Failed to download file for PTA: " pta ". Error: " (.getMessage e)))
        nil))
    (do
      (println (str "Invalid PTA: " pta ". Must be one of: " (keys netex-zip-files)))
      nil)))

(comment

  (download-netex-zip :SOF)
  (download-netex-zip :SKY)
  (download-netex-zip :KOL)
  (download-netex-zip :OST)

  (download-netex-zip :MOR)
  (download-netex-zip :OST)
  (download-netex-zip :NOR)
  (download-netex-zip :OST)
  (download-netex-zip :INVALID)  ; Should show invalid PTA message

  ;; --->
  )
