(ns com.gfredericks.git-git.io
  (:refer-clojure :exclude [println])
  (:require [clojure.string :as s]
            [me.raynes.fs :as fs]
            [me.raynes.conch :refer [programs with-programs let-programs]]
            [robert.hooke :refer [add-hook]]))

(def ^:dynamic *dry-run?* false)
(def ^:dynamic *quiet?* false)

(defn ^:private println
  [& args]
  (when-not *quiet?*
    (apply clojure.core/println args)))

(defn ^:private conch-fn-hook
  "Hooks a conch function so that it throws on error on failure and
  returns stdout otherwise."
  [f & args]
  (let [{:keys [exit-code stderr stdout]}
        ;; do we need to check if the last arg is a map??
        (apply f (concat args [{:verbose true}]))]
    (when (pos? @exit-code)
      (throw (ex-info "Subprocess failed!"
                      {:command-args args
                       :stderr stderr
                       :command (or (-> f meta :command-name) (str f))})))
    stdout))

(let-programs [git* "git"]
  (def ^:private git* (vary-meta git* assoc :command-name "git")))
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

(defn read-remotes
  [dir]
  (-> (git-RO "remote" "-v" :dir dir)
      (s/split #"\n")
      (->> (map #(s/split % #"\s"))
           (filter #(= "(fetch)" (last %)))
           (map #(vec (take 2 %)))
           (into {}))))

(defn read-branches
  [dir]
  (fs/with-cwd (fs/file dir ".git/refs/heads")
    (into {}
     (for [branch-name (fs/list-dir fs/*cwd*)]
       [branch-name (.trim ^String (slurp (fs/file fs/*cwd* branch-name)))]))))

(defn git-clone
  [origin target-dir]
  ;; I don't think we need to set :dir here
  (git "clone" origin (str target-dir)))

(defn git-fetch
  [remote-name cwd]
  (git "fetch" remote-name :dir cwd))

(defn git-add-remote
  [remote-name url cwd]
  (git "remote" "add" remote-name url :dir cwd))
