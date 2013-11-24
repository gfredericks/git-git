(ns com.gfredericks.git-git
  (:require [clojure.tools.cli :refer [cli]]
            [com.gfredericks.git-git.actions :as actions]
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
  [& args]
  (let [[opts [verb :as moreargs] help]
        (cli args
             ["-q" "--quiet" "Don't report changes made" :flag true]
             ["--dry-run" "Report what would be done without doing it" :flag true])]
    (try
      (if-let [f (dispatch verb)]
        (binding [cfg/*quiet?* (:quiet opts)
                  cfg/*dry-run?* (:dry-run opts)]
          (f (cfg/config)))
        (println (str usage "\n\n" help)))
      (finally
        (shutdown-agents)))))
