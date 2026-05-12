(ns user
  (:require
   [jansenh.transmodel.core :as core]
   [clojure.repl :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.tools.namespace.repl :refer [refresh refresh-all]]))

;; Optional: Auto-refresh on file changes
;; (add-hook! :repl/after-load refresh)

(defn dev []
  (println "Dev environment loaded. Use (refresh) to reload code."))
