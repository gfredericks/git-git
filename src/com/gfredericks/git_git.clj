(ns com.gfredericks.git-git
  (:require [com.gfredericks.git-git.actions :as actions]
            [com.gfredericks.git-git.config :as cfg])
  (:gen-class))

(defn update-from-local
  [cfg]
  (->> (actions/determine-actions cfg)
       (remove actions/effecting-local?)
       (actions/perform-all cfg)))

(defn sync-to-local
  [cfg]
  (->> (actions/determine-actions cfg)
       (remove actions/effecting-registry?)
       (actions/perform-all cfg)))

(defn status
  [cfg]
  (doseq [act (actions/determine-actions cfg)]
    (println act)))

;;;;;;;;;;
;; main ;;
;;;;;;;;;;

(def dispatch
  {"sync-to-local" sync-to-local
   "update-from-local" update-from-local
   "status" status})

(def usage
  "Usage:

cmd [status | sync-to-local | update-from-local]

The repo directory and config file will be inferred from
context, or can be set with the GIT_GIT_DIR and GIT_GIT_FILE
environment variables respectively.")


(defn -main
  [& [verb]]
  (try
    (if-let [f (dispatch verb)]
      (f (cfg/config))
      (println usage))
    (finally
      (shutdown-agents))))
