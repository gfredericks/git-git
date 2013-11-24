(ns com.gfredericks.git-git.config
  (:require [environ.core :refer [env]]
            [me.raynes.fs :as fs]))

(def ^:dynamic *dry-run?* false)
(def ^:dynamic *quiet?* false)

(defn ^:private repo-dir-from-env
  []
  (some-> (env :git-git-dir)
          (fs/file)))

(defn ^:private repo-dir-from-cwd
  []
  (->> (cons fs/*cwd* (fs/parents fs/*cwd*))
       (filter #(fs/exists? (fs/file % ".git-git.clj")))
       (first)))

(defn determine-repo-dir
  "Figure out the repo directory by checking env variables and the
  process's CWD. Returns a File object."
  []
  (or (repo-dir-from-env)
      (repo-dir-from-cwd)
      (throw (ex-info "Can't determine repo directory!" {}))))

(defn determine-config-file
  ""
  [repo-dir]
  (or (some-> (env :git-git-file) (fs/file))
      (fs/file repo-dir ".git-git.clj")))

(defn config
  "Returns a map with :file and :dir."
  []
  (let [dir (determine-repo-dir)]
    {:dir dir
     :file (determine-config-file dir)}))
