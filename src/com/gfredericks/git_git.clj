(ns com.gfredericks.git-git
  (:require [clojure.core.typed :refer :all]
            [clojure.tools.cli :refer [cli]]
            [com.gfredericks.git-git.actions :as actions]
            [com.gfredericks.git-git.config :as cfg])
  (:import (clojure.lang IPersistentMap IPersistentVector ISeq))
  (:gen-class))

(ann show [actions/Action -> String])
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

(ann update-from-local [cfg/Config -> Any])
(defn update-from-local
  [cfg]
  (->> (actions/determine-actions cfg)
       (remove actions/effecting-local?)
       (actions/perform-all cfg)))

(ann sync-to-local [cfg/Config -> Any])
(defn sync-to-local
  [cfg]
  (->> (actions/determine-actions cfg)
       (remove actions/effecting-registry?)
       (actions/perform-all cfg)))

(ann print-in-a-box [String -> nil])
(defn print-in-a-box
  [s]
  (let [line (str \+ (apply str (repeat (+ 2 (count s)) \-)) \+)]
    (println line)
    (println \| s \|)
    (println line)))

(ann ^:no-check clojure.core/group-by
     (All [a b] [[a -> b] (Seqable a) -> (IPersistentMap b (IPersistentVector a))]))
(ann ^:no-check clojure.core/sort-by
     ;; Ignoring the 3-arity case here
     (All [a b] [[a -> b] (Seqable a) -> (ISeq a)]))

(ann ^:no-check status [cfg/Config -> Any]) ; can't figure this one out
(defn status
  [cfg]
  (doseq> [[repo-name actions] :- (clojure.lang.IMapEntry String (IPersistentVector actions/Action))
           (->> (actions/determine-actions cfg)
                (group-by :repo-name)
                (sort-by first))]
    (print-in-a-box repo-name)
    (doseq> [act :- actions/Action actions]
      (println (show act)))
    (println)))

;;;;;;;;;;
;; main ;;
;;;;;;;;;;

(ann dispatch (IPersistentMap String [cfg/Config -> Any]))
(def dispatch
  {"sync-to-local"     sync-to-local
   "update-from-local" update-from-local
   "status"            status
   "fetch"             actions/fetch-all})

(ann usage String)
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

(ann ^:no-check -main [String * -> Any])
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
