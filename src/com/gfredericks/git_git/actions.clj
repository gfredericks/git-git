(ns com.gfredericks.git-git.actions
  "Check what things can be done and do them."
  (:require [clojure.data :refer [diff]]
            [clojure.java.io :as jio]
            [clojure.pprint :as pp]
            [com.gfredericks.git-git.config :as cfg]
            [com.gfredericks.git-git.io :as io]
            [com.gfredericks.git-git.util :refer [canonize pdoseq]]
            [me.raynes.fs :as fs]))

(defn ^:private diff-sets
  [coll1 coll2]
  (diff (set coll1) (set coll2)))

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

(defmulti effecting-local? :type)
(defmethod effecting-local? ::effecting-local [_] true)
(defmethod effecting-local? :default [_] false)

(defmulti effecting-registry? :type)
(defmethod effecting-registry? ::effecting-registry [_] true)
(defmethod effecting-registry? :default [_] false)


;;;;;;;;;;;;;;;;;;;
;; determination ;;
;;;;;;;;;;;;;;;;;;;

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

(defn determine-actions-for-branch
  [repo-dir branch-name local-sha registry-sha]
  (when (not= local-sha registry-sha)
    [{:type (if (io/git-branch-contains? repo-dir branch-name registry-sha)
              ::unregistered-commits
              ::unmerged-commits)
      :local-sha local-sha
      :registry-sha registry-sha
      :branch-name branch-name}]))

(defn determine-actions-for-repo
  [repo-dir repo-registry-data]
  (let [local-branches (io/read-branches repo-dir)
        registry-branches (:branches repo-registry-data)

        [unregistered-branches
         unlocal-branches
         common-branches]
        (diff-sets (keys local-branches)
                   (keys registry-branches))]
    (concat
     (for [branch-name unregistered-branches]
       {:type ::unregistered-branch
        :branch-name branch-name
        :branch-head (get local-branches branch-name)})
     (for [branch-name unlocal-branches]
       {:type ::untracked-branch
        :branch-name branch-name
        :branch-head (get registry-branches branch-name)})
     (for [branch-name common-branches
           action (determine-actions-for-branch
                   repo-dir
                   branch-name
                   (get local-branches branch-name)
                   (get-in repo-registry-data [:branches branch-name]))]
       action))))

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
     (for [repo-name unregistered-repos]
       {:type ::unregistered-repo
        :repo-name repo-name})
     (for [repo-name uncloned-repos]
       {:type ::uncloned-repo
        :repo-name repo-name})
     (for [repo-name common-repos
           action (determine-actions-for-repo (fs/file dir repo-name)
                                              (get registry-repos repo-name))]
       (assoc action :repo-name repo-name)))))

;;;;;;;;;;;;;;;;;
;; performance ;;
;;;;;;;;;;;;;;;;;

(defmulti perform
  "Returns possibly modified registry data."
  (fn [cfg registry-data action] (:type action)))

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
  (let [remotes (get-in registry-data [:repos repo-name :remotes])
        dir (repo-dir cfg repo-name)]
    (if-let [origin (get remotes "origin")]
      (io/git-clone origin dir)
      (throw (ex-info "No origin in registry!" {:repo-name repo-name})))
    (doseq [[remote-name url] (dissoc remotes "origin")]
      (io/git-add-remote remote-name url dir)))
  registry-data)

(defn ^:private availabalize-commit
  "Checks if the sha is in the repo, tries to fetch if not, and
  throws an exception if it can't be found."
  [repo-dir commit-sha]
  (or (io/git-repo-has-commit? repo-dir commit-sha)
      (doseq [[remote-name _uri] (io/read-remotes repo-dir)]
        (io/git-fetch remote-name repo-dir))
      (io/git-repo-has-commit? repo-dir commit-sha)
      (throw (ex-info "Can't obtain commit!"
                      {:repo-dir repo-dir
                       :commit-sha commit-sha}))))

(defmethod perform ::untracked-branch
  [cfg registry-data {:keys [repo-name branch-name branch-head]
                      :as action}]
  (let [dir (repo-dir cfg repo-name)]
    (availabalize-commit dir branch-head)
    (io/git-branch dir branch-name branch-head))
  registry-data)

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
  :ok)


(defn fetch-all
  [{:keys [dir]}]
  (pdoseq [repo-name (io/existing-repos dir)]
    (let [dir (fs/file dir repo-name)]
      (doseq [[remote-name] (io/read-remotes dir)]
        (io/git-fetch remote-name dir)))))
