(ns com.gfredericks.git-git.actions
  "Check what things can be done and do them."
  (:refer-clojure :exclude [assoc-in])
  (:require [clojure.core.typed :refer :all]
            [clojure.data :refer [diff]]
            [clojure.java.io :as jio]
            [clojure.pprint :as pp]
            [com.gfredericks.git-git.config :as cfg]
            [com.gfredericks.git-git.io :as io]
            [com.gfredericks.git-git.util :refer [assoc-in canonize pdoseq]]
            [me.raynes.fs :as fs])
  (:import (clojure.lang ExceptionInfo IPersistentCollection IPersistentMap IPersistentSet ISeq)
           (java.io File)))

(def-alias UnmergedCommits
  (HMap :mandatory {:type         (Value ::unmerged-commits)
                    :local-sha    io/SHA
                    :registry-sha io/SHA
                    :branch-name  io/Branch
                    :repo-name    String}))
(def-alias UnregisteredCommits
  (HMap :mandatory {:type         (Value ::unregistered-commits)
                    :local-sha    io/SHA
                    :registry-sha io/SHA
                    :branch-name  io/Branch
                    :repo-name    String}))
(def-alias UnregisteredBranch
  (HMap :mandatory {:type        (Value ::unregistered-branch)
                    :branch-name io/Branch
                    :branch-head io/SHA
                    :repo-name   String}))
(def-alias UntrackedBranch
  (HMap :mandatory {:type        (Value ::untracked-branch)
                    :branch-name io/Branch
                    :branch-head io/SHA
                    :repo-name   String}))
(def-alias UnregisteredRepo
  (HMap :mandatory {:type      (Value ::unregistered-repo)
                    :repo-name String}))
(def-alias UnclonedRepo
  (HMap :mandatory {:type      (Value ::uncloned-repo)
                    :repo-name String}))
(def-alias Action (U UnregisteredCommits UnmergedCommits
                     UnregisteredBranch  UntrackedBranch
                     UnregisteredRepo    UnclonedRepo))

(def-alias RepoRegistry (HMap :mandatory {:branches (IPersistentMap io/Branch io/SHA)
                                          :remotes (IPersistentMap String String)}))
(def-alias Registry (HMap :mandatory {:repos (IPersistentMap String RepoRegistry)}))

(ann diff-sets (All [a] [(Seqable a) (Seqable a) ->
                         (Vector* (IPersistentSet a)
                                  (IPersistentSet a)
                                  (IPersistentSet a))]))
(defn ^:private diff-sets
  [coll1 coll2]
  (diff (set coll1) (set coll2)))

(ann safe-get (All [k v] [(IPersistentMap k v) k -> v]))
(defn ^:private safe-get [m k] (if-let [e (find m k)] (key e) (throw (ex-info "Can't find key!" {:k k, :m m}))))

;;;;;;;;;;;
;; types ;;
;;;;;;;;;;;

(defn derives
  [parent & tags]
  (doseq [tag tags] (derive tag parent)))

(derives ::effecting-local
         ::unmerged-commits
         ::untracked-branch
         ::uncloned-repo)

(derives ::effecting-registry
         ::unregistered-commits
         ::unregistered-branch
         ::unregistered-repo)

(ann effecting-local? [Action -> Boolean])
(defmulti effecting-local? :type)
(defmethod effecting-local? ::effecting-local [_] true)
(defmethod effecting-local? :default [_] false)

(ann effecting-registry? [Action -> Boolean])
(defmulti effecting-registry? :type)
(defmethod effecting-registry? ::effecting-registry [_] true)
(defmethod effecting-registry? :default [_] false)


;;;;;;;;;;;;;;;;;;;
;; determination ;;
;;;;;;;;;;;;;;;;;;;

(ann read-registry-file [File -> Registry])
(defn read-registry-file
  [file]
  (when (fs/exists? file)
    (with-open [r (jio/reader file)
                r' (java.io.PushbackReader. r)]
      (read r' false nil))))

(defn read-repo-data
  [dir]
  {:remotes (io/read-remotes dir)
   :branches (io/read-branches dir)})

(defn read-repo-directory-data
  [{:keys [dir] :as cfg}]
  {:pre [dir (fs/directory? dir)]}
  {:repos
   (->> (fs/list-dir dir)
        (map (partial fs/file dir))
        (filter io/git-repo?)
        (map (fn [dir]
               [(fs/base-name dir) (read-repo-data dir)]))
        (into {}))})

(ann determine-actions-for-branch [String io/Repo io/Branch io/SHA io/SHA -> (IPersistentCollection Action)])
(defn determine-actions-for-branch
  [repo-name repo-dir branch-name local-sha registry-sha]
  (if (not= local-sha registry-sha)
    ;; this is horrendous
    (if (io/git-branch-contains? repo-dir branch-name registry-sha)
        [{:type         ::unregistered-commits
          :local-sha    local-sha
          :registry-sha registry-sha
          :branch-name  branch-name
          :repo-name    repo-name}]
        [{:type         ::unmerged-commits
          :local-sha    local-sha
          :registry-sha registry-sha
          :branch-name  branch-name
          :repo-name    repo-name}])
    []))

(ann determine-actions-for-repo [String File RepoRegistry -> (ISeq Action)])
(defn determine-actions-for-repo
  [repo-name repo-dir repo-registry-data]
  (let [local-branches (io/read-branches repo-dir)
        registry-branches (:branches repo-registry-data)

        [unregistered-branches
         unlocal-branches
         common-branches]
        (diff-sets (keys local-branches)
                   (keys registry-branches))]
    (concat
     (for> :- Action [branch-name :- io/Branch unregistered-branches]
           {:type        ::unregistered-branch
            :branch-name branch-name
            :branch-head (safe-get local-branches branch-name)
            :repo-name   repo-name})
     (for> :- Action [branch-name :- io/Branch unlocal-branches]
           {:type        ::untracked-branch
            :branch-name branch-name
            :branch-head (safe-get registry-branches branch-name)
            :repo-name   repo-name})
     (for> :- Action [branch-name :- io/Branch common-branches
                      action :- Action (determine-actions-for-branch
                                        repo-name
                                        repo-dir
                                        branch-name
                                        (safe-get local-branches branch-name)
                                        (safe-get registry-branches branch-name))]
           action))))

(ann determine-actions [cfg/Config -> (ISeq Action)])
(defn determine-actions
  "Returns a collection of actions."
  [{:keys [file dir] :as cfg}]
  (let [registry (read-registry-file file)
        registry-repos (:repos registry)

        [unregistered-repos
         uncloned-repos
         common-repos]
        (diff-sets (io/existing-repos dir)
                   (keys registry-repos))]
    (concat
     (for> :- Action [repo-name :- String unregistered-repos]
           {:type ::unregistered-repo
            :repo-name repo-name})
     (for> :- Action [repo-name :- String uncloned-repos]
           {:type ::uncloned-repo
            :repo-name repo-name})
     (for> :- Action [repo-name :- String common-repos
                      action :- Action (determine-actions-for-repo repo-name
                                                                   (fs/file dir repo-name)
                                                                   (safe-get registry-repos repo-name))]
           action))))

;;;;;;;;;;;;;;;;;
;; performance ;;
;;;;;;;;;;;;;;;;;

(ann ^:yes-check perform [cfg/Config Registry Action -> Registry])
(defmulti perform
  "Returns possibly modified registry data."
  (fn [_cfg _registry-data action] (:type action)))

(ann repo-dir [cfg/Config String -> File])
(defn repo-dir
  [{:keys [dir]} repo-name]
  (fs/file dir repo-name))

(defmethod perform ::unregistered-repo
  [cfg registry-data {:keys [repo-name]}]
  (let [dir (repo-dir cfg repo-name)]
    (assoc-in registry-data [:repos repo-name]
              {:remotes (io/read-remotes dir)
               :branches (io/read-branches dir)})))

(defmethod perform ::unregistered-branch
  [cfg registry-data {:keys [repo-name
                             branch-name
                             branch-head]}]
  (assoc-in registry-data
            [:repos repo-name :branches branch-name]
            branch-head))

(defmethod perform ::unregistered-commits
  [cfg registry-data {:keys [repo-name
                             branch-name
                             local-sha]}]
  (assoc-in registry-data
            [:repos repo-name :branches branch-name]
            local-sha))

(defmethod perform ::uncloned-repo
  [cfg registry-data {:keys [repo-name]}]
  (let [remotes (-> registry-data :repos (get repo-name) :remotes)
        dir (repo-dir cfg repo-name)]
    (if-let [origin (get remotes "origin")]
      (io/git-clone origin dir)
      (throw (ex-info "No origin in registry!" {:repo-name repo-name})))
    (doseq> [[remote-name url] :- (Vector* String String) remotes
             :when (not= remote-name "origin")]
      (io/git-add-remote remote-name url dir)))
  registry-data)

(ann availabalize-commit [io/Repo io/SHA -> nil])
(defn ^:private availabalize-commit
  "Checks if the sha is in the repo, tries to fetch if not, and
  throws an exception if it can't be found."
  [repo-dir commit-sha]
  (or (io/git-repo-has-commit? repo-dir commit-sha)
      (doseq> [[remote-name _uri] :- (Vector* io/Remote Any)
               (io/read-remotes repo-dir)]
        (io/git-fetch remote-name repo-dir))
      (io/git-repo-has-commit? repo-dir commit-sha)
      (throw (ex-info "Can't obtain commit!"
                      {:repo-dir repo-dir
                       :commit-sha commit-sha})))
  nil)

(defmethod perform ::untracked-branch
  [cfg registry-data {:keys [repo-name branch-name branch-head]
                      :as action}]
  (let [dir (repo-dir cfg repo-name)]
    (availabalize-commit dir branch-head)
    (io/git-branch dir branch-name branch-head))
  registry-data)

(ann clojure.core/spit [File String -> nil])
(ann clojure.core/slurp [File -> String])
(ann clojure.core/printf [String Any * -> nil])
(ann me.raynes.fs/exists? [File -> Boolean])
(ann hard-branch-set [io/Repo io/Branch io/SHA -> nil])
(defn ^:private hard-branch-set
  "Writes the SHA to the branch's file."
  [repo-dir branch-name sha]
  (let [ref-file (fs/file repo-dir
                          ".git/refs/heads"
                          branch-name)]
    (assert (fs/exists? ref-file))
    (when-not cfg/*quiet?*
      (printf "Fast-forwarding branch %s from %s -> %s\n"
              branch-name
              (.trim ^String (slurp ref-file))
              sha))
    (when-not cfg/*dry-run?*
      (spit ref-file (str sha "\n")))))

(ann ^:no-check clojure.core/ex-info [String (HMap) -> ExceptionInfo])

(defmethod perform ::unmerged-commits
  [cfg registry-data {:keys [repo-name branch-name
                             local-sha registry-sha]}]
  (let [dir (repo-dir cfg repo-name)]
    (availabalize-commit dir registry-sha)
    (if (io/fast-forward? dir branch-name registry-sha)
      (if (io/branch-checked-out? dir branch-name)
        (if (io/clean-repo? dir)
          (io/git-merge dir registry-sha)
          (throw (ex-info "Can't update branch while checked out with dirty repo!"
                          {:repo-name repo-name
                           :branch-name branch-name})))
        (hard-branch-set dir branch-name registry-sha))
      (throw (ex-info "Updating branch requires merge!"
                      {:repo-name repo-name
                       :branch-name branch-name}))))
  registry-data)

;; this one crashes the type checker??!
(ann ^:no-check perform-all [cfg/Config (ISeq Action) -> nil])
(defn perform-all
  [{registry-file :file, :as cfg} actions]
  (let [registry-data (read-registry-file registry-file)
        registry-data'
        (reduce (partial perform cfg)
                registry-data
                actions)]
    (when-not cfg/*dry-run?*
      (when (not= registry-data registry-data')
        (with-open [w (jio/writer registry-file)]
          (binding [*out* w]
            (-> registry-data'
                (canonize)
                (pp/pprint)))))))
  nil)

(ann ^:no-check me.raynes.fs/file [Any * -> File])

(ann fetch-all [cfg/Config -> nil])
(defn fetch-all
  [{:keys [dir]}]
  (pdoseq [repo-name (io/existing-repos dir)]
    (let [dir (fs/file dir repo-name)]
      (doseq> [[remote-name] :- (Vector* String Any *) (io/read-remotes dir)]
        (io/git-fetch remote-name dir)))))
