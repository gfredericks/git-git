(ns com.gfredericks.git-git
  (:require [clojure.tools.cli :refer [cli]]
            [com.gfredericks.git-git.actions :as actions]
            [com.gfredericks.git-git.config :as cfg])
  (:gen-class))

(defmulti show
  "Returns a human-friendly description of the action"
  :type)

(defmethod show ::actions/unmerged-commits
  [{:keys [branch-name]}]
  (str "Unmerged commits on: " branch-name))

(defmethod show ::actions/untracked-branch
  [{:keys [branch-name]}]
  (str "Untracked branch: " branch-name))

(defmethod show ::actions/uncloned-repo
  [_]
  (str "Uncloned!"))

(defmethod show ::actions/unregistered-commits
  [{:keys [branch-name]}]
  (str "Unregistered commits on: " branch-name))

(defmethod show ::actions/unregistered-branch
  [{:keys [branch-name]}]
  (str "Unregistered branch: " branch-name))

(defmethod show ::actions/unregistered-repo
  [_]
  (str "Unregistered!"))

(defmethod show :default [act] (pr-str act))

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

(defn print-in-a-box
  [s]
  (let [line (str \+ (apply str (repeat (+ 2 (count s)) \-)) \+)]
    (println line)
    (println \| s \|)
    (println line)))

(defn status
  [cfg]
  (doseq [[repo-name actions]
          (->> (actions/determine-actions cfg)
               (group-by :repo-name)
               (sort-by first))]
    (print-in-a-box repo-name)
    (doseq [act actions]
      (println (show act)))
    (println)))

;;;;;;;;;;
;; main ;;
;;;;;;;;;;

(def dispatch
  {"sync-to-local"     sync-to-local
   "update-from-local" update-from-local
   "status"            status
   "fetch"             actions/fetch-all})

(def usage
  "Usage: [opts ...] command
  where command can be:

    status:            show discrepancies between filesystem and registry
    sync-to-local:     make changes to filesystem
    update-from-local: make changes to registry
    fetch:             run `git fetch` in all repos (and all remotes) in the filesystem

  The repo directory and config file will be inferred from
  context, or can be set with the GIT_GIT_DIR and GIT_GIT_FILE
  environment variables respectively.")


(defn -main
  [& args]
  (let [[opts [verb :as moreargs] help]
        (cli args
             ["-q" "--quiet" "Don't report changes made" :flag true]
             ["--dry-run" "Report what would be done without doing it" :flag true])

        f (dispatch (if (empty? moreargs) "status" verb))]
    (when (empty? moreargs)
      (println "(showing status; use the \"help\" command for more options)\n"))
    (try
      (if (or (= "help" verb) (nil? f))
        (do
          (when (and verb (not= verb "help"))
            (println "Unrecognized command:" verb))
          (println (str "  " usage "\n\n" help)))
        (binding [cfg/*quiet?* (:quiet opts)
                  cfg/*dry-run?* (:dry-run opts)]
          (f (cfg/config))))
      (finally
        (shutdown-agents)))))
