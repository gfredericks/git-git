(ns com.gfredericks.git-git
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as s]
            [com.gfredericks.git-git.util :refer [canonize]]
            [me.raynes.fs :as fs]
            [me.raynes.conch :refer [programs with-programs let-programs]]
            [robert.hooke :refer [add-hook]])
  (:gen-class))

(programs git)
(add-hook #'git ::log
          (fn [orig & [verb :as args]]
            (when-not (#{"remote"} verb)
              (println (cons 'git (take-while string? args))))
            (apply orig args)))

(defn git-repo?
  [dir]
  (and (fs/directory? dir)
       (fs/exists? (fs/file dir ".git"))))

(defn read-remotes
  [dir]
  (-> (git "remote" "-v" :dir dir)
      (s/split #"\n")
      (->> (map #(s/split % #"\s"))
           (filter #(= "(fetch)" (last %)))
           (map #(vec (take 2 %)))
           (into {}))))

(defn read-repo-data
  [dir]
  {:remotes (read-remotes dir)})

(defn read-repo-directory-data
  [{:keys [dir] :as cfg}]
  {:pre [dir (fs/directory? dir)]}
  {:repos
   (->> (fs/list-dir dir)
        (map (partial fs/file dir))
        (filter git-repo?)
        (map (fn [dir]
               [(fs/name dir) (read-repo-data dir)]))
        (into {}))})

(defn update-from-local
  [{:keys [file] :as cfg}]
  {:pre [(:dir cfg)]}
  (let [data (read-repo-directory-data cfg)]
    (binding [*out* (io/writer file)]
      (-> data canonize pp/pprint))))

(defn sync-to-local**
  [dir data]
  (if (fs/exists? dir)
    (git "fetch" :dir dir)
    (git "clone" (get-in data [:remotes "origin"]) (str dir) :dir (fs/parent dir))))

(defn sync-to-local*
  [data cfg]
  (doseq [[repo-name data] (:repos data)]
    (sync-to-local** (fs/file (:dir cfg) repo-name) data)))

(defn sync-to-local
  [{:keys [file] :as cfg}]
  (sync-to-local* (-> file slurp read-string) cfg))

;;;;;;;;;;
;; main ;;
;;;;;;;;;;

(def dispatch
  {"sync-to-local" sync-to-local
   "update-from-local" update-from-local})

(defn parse-config-args
  [args]
  (for [s args
        :let [[_ dir _ file] (re-matches #"(.*?)(:(.*))?" s)]]
    {:file (if file
             (fs/file file)
             (fs/file dir ".git-git.clj"))
     :dir (fs/file dir)}))

(def usage
  "Usage:

cmd [sync-to-local | update-from-local] cfgs*

cfg: some-dir (config defaults to some-dir/.git-git.clj)
     some-dir:some-config.clj")

(defn -main
  [& [verb & cfgs :as args]]
  (let [f (dispatch verb)]
    (if (or (empty? args) (nil? f))
      (println usage)
      (doseq [cfg (parse-config-args cfgs)]
        (println (format "Processing %s..." (str (:dir cfg))))
        (f cfg))))
  (shutdown-agents))
