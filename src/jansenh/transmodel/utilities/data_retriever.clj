;;-----------------------------------------------------------------------------
;; File: src/jansenh/transmodel/utilities/data_retriever.clj
;; Author: Henning Jansen - henning.jansen@jansenh.no
;; Copyright: (c) 2025
;; License: Eclipse Public License 2.0 - http://www.eclipse.org/legal/epl-2.0.
;;
;;-----------------------------------------------------------------------------

(ns jansenh.transmodel.utilities.data-retriever)

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
