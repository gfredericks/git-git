(ns com.gfredericks.git-git.io
  (:refer-clojure :exclude [println])
  (:require [clojure.core.typed :refer :all]
            [clojure.string :as s]
            [com.gfredericks.git-git.config :refer [*dry-run?* *quiet?*]]
            [me.raynes.fs :as fs]
            [me.raynes.conch :refer [programs with-programs let-programs]]
            [robert.hooke :refer [add-hook]])
  (:import (clojure.lang IPersistentMap ISeq)
           (java.io File)))

(def-alias Repo File)
(def-alias Branch String)
(def-alias Remote String) ; is this the name or the URL??
(def-alias SHA String)

(defn ^:private println
  [& args]
  (when-not *quiet?*
    (apply clojure.core/println args)))

(defn ^:private conch-fn-hook
  "Hooks a conch function so that it throws on error on failure and
  returns stdout otherwise."
  [f & args]
  (let [args' (if (map? (last args))
                (concat (butlast args) [(assoc (last args) :verbose true)])
                (concat args [{:verbose true}]))

        {:keys [exit-code stderr stdout]}
        (apply f args')]
    (when (pos? @exit-code)
      (throw (ex-info "Subprocess failed!"
                      {:command-args args
                       :stderr stderr
                       :command (or (-> f meta :command-name) (str f))})))
    stdout))

(let-programs [git* "git"]
  (def ^:private git* (vary-meta git* assoc :command-name "git"))
  (def ^:private git-RO-raw git*))
(add-hook #'git* ::conch conch-fn-hook)

(defn git
  [& args]
  (apply println "git" (take-while string? args))
  (when-not *dry-run?* (apply git* args)))

(defn git-RO
  "Like git but runs even when *dry-run?* is true."
  [& args]
  (apply git* args))

(defn git-repo?
  "Checks if the given directory has a .git directory in it."
  [dir]
  (and (fs/directory? dir)
       (fs/directory? (fs/file dir ".git"))))

(ann existing-repos [File -> (ISeq String)])
(defn existing-repos
  "Returns a list of subdirectory names that are git repos."
  [dir]
  (->> (fs/list-dir dir)
       (map (partial fs/file dir))
       (filter git-repo?)
       (map fs/base-name)))

(ann read-remotes
     [File -> (IPersistentMap String String)])
(defn read-remotes
  [dir]
  (->> (git-RO "remote" "-v" {:seq true, :dir dir})
       (map #(s/split % #"\s"))
       (filter #(= "(fetch)" (last %)))
       (map #(vec (take 2 %)))
       (into {})))

(defn branch->sha
  [repo-dir branch-name]
  (.trim ^String (slurp (fs/file repo-dir
                                 ".git/refs/heads"
                                 branch-name))))

(ann read-branches [Repo -> (IPersistentMap Branch SHA)])
(defn read-branches
  [dir]
  (into {}
        (for [branch-name (fs/list-dir (fs/file dir ".git/refs/heads"))]
          [branch-name (branch->sha dir branch-name)])))

(ann git-clone [String File -> nil])
(defn git-clone
  [origin target-dir]
  ;; I don't think we need to set :dir here
  (git "clone" origin (str target-dir)))

(ann git-fetch [String File -> nil])
(defn git-fetch
  [remote-name cwd]
  (git "fetch" remote-name :dir cwd))

(ann git-add-remote [String String File -> nil])
(defn git-add-remote
  [remote-name url cwd]
  (git "remote" "add" remote-name url :dir cwd))

(ann git-repo-has-commit? [Repo SHA -> Boolean])
(defn git-repo-has-commit?
  "Checks if the git repo has the object"
  [repo-dir sha-to-check]
  (let [{:keys [stdout ^String stderr], :as data}
        (git-RO-raw "cat-file" "-t" sha-to-check
                    :dir repo-dir
                    {:verbose true})]
    (cond (= "commit\n" stdout) true

          ;; it gives this response for sha prefixes
          (.contains stderr "Not a valid object name") false
          ;; and this one for full-length SHAs
          (.contains stderr "unable to find") false
          ;; and this for full lunch SHAs in newer versions
          (.contains stderr "bad file") false

          :else (throw (ex-info "Confusing response from git-cat-file"
                                {:response     data
                                 :repo-dir     repo-dir
                                 :sha-to-check sha-to-check})))))

(defn git-branch-contains?
  "Checks if the branch in the given repo contains the given SHA."
  [repo-dir branch-name sha-to-check]
  (and (git-repo-has-commit? repo-dir sha-to-check)
       (let [branch-bullets (git-RO "branch" "--contains" sha-to-check {:dir repo-dir, :seq true})]
         (boolean (some #{(str "* " branch-name)} branch-bullets)))))

(ann git-branch [Repo Branch SHA -> nil])
(defn git-branch
  [repo-dir branch-name start-sha]
  (git "branch" branch-name start-sha :dir repo-dir))

(ann fast-forward? [Repo Branch SHA -> Boolean])
(defn fast-forward?
  "Checks if the given branch can be fast-forwarded to the given SHA."
  [repo-dir branch-name commit-sha]
  (let [branch-sha (branch->sha repo-dir branch-name)
        ^String merge-base-sha (git-RO "merge-base"
                                       branch-name
                                       commit-sha
                                       :dir repo-dir)]
    (.startsWith merge-base-sha branch-sha)))

(ann branch-checked-out? [Repo Branch -> Boolean])
(defn branch-checked-out?
  [repo-dir branch-name]
  (let [s (.trim ^String (slurp (fs/file repo-dir ".git/HEAD")))]
    ;; I think there might be extreme edge cases where this gives a
    ;; false positive but who cares
    (.endsWith s (str "/" branch-name))))

(defn ^:private exit-code
  "Returns true if exit code is 0 and false otherwise."
  [{:keys [exit-code]}]
  (zero? @exit-code))

(defn unstaged-changes?
  [repo-dir]
  (-> (git-RO-raw "diff" "--exit-code"
                  {:verbose true, :dir repo-dir})
      (exit-code)
      (not)))

(defn staged-changes?
  [repo-dir]
  (-> (git-RO-raw "diff" "--cached" "--exit-code"
                  {:verbose true, :dir repo-dir})
      (exit-code)
      (not)))

(defn untracked-files?
  [repo-dir]
  (-> (git-RO "ls-files" "--other" "--exclude-standard" {:dir repo-dir})
      (empty?)
      (not)))

(ann clean-repo? [Repo -> Boolean])
(defn clean-repo?
  "Returns true if the repo has no staged or unstaged changes or
  untracked files."
  [repo-dir]
  (not ((some-fn unstaged-changes? staged-changes? untracked-files?) repo-dir)))

(ann git-merge [Repo SHA -> nil])
(defn git-merge
  [repo-dir merge-to]
  (git "merge" merge-to {:dir repo-dir}))
