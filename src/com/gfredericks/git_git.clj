(ns com.gfredericks.git-git
  (:require [clojure.data :refer [diff]]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as s]
            [com.gfredericks.git-git.config :as cfg]
            [com.gfredericks.git-git.util :refer [canonize pdoseq]]
            [me.raynes.fs :as fs]
            [me.raynes.conch :refer [programs with-programs let-programs]]
            [robert.hooke :refer [add-hook]])
  (:gen-class))

;; can't remember why I couldn't use add-hook instead of this
;; nonsense.
(let-programs [git* "git"]
  (defn git [& args]
    (when (not (#{"remote" "fetch"} (first args)))
      (apply println "git" (take-while string? args)))
    (let [{:keys [exit-code stderr stdout]}
          (apply git* (concat args
                              [{:verbose true}]))]
      (when (pos? @exit-code)
        (throw (ex-info "Git subprocess failed!"
                        {:command-args args
                         :stderr stderr})))
      stdout)))

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

(defn read-branches
  [dir]
  (fs/with-cwd (fs/file dir ".git/refs/heads")
    (into {}
     (for [branch-name (fs/list-dir fs/*cwd*)]
       [branch-name (.trim ^String (slurp (fs/file fs/*cwd* branch-name)))]))))

(defn read-repo-data
  [dir]
  {:remotes (read-remotes dir)
   :branches (read-branches dir)})

(defn read-repo-directory-data
  [{:keys [dir] :as cfg}]
  {:pre [dir (fs/directory? dir)]}
  {:repos
   (->> (fs/list-dir dir)
        (map (partial fs/file dir))
        (filter git-repo?)
        (map (fn [dir]
               [(fs/base-name dir) (read-repo-data dir)]))
        (into {}))})

(defn update-from-local
  [{:keys [file], :as cfg}]
  {:pre [(:dir cfg)]}
  (let [data (read-repo-directory-data cfg)]
    (binding [*out* (io/writer file)]
      (-> data canonize pp/pprint))))

(defn sync-to-local**
  [dir data]
  (when-not (fs/exists? dir)
    (git "clone" (get-in data [:remotes "origin"]) (str dir) :dir (fs/parent dir)))
  (let [existing-remotes (read-remotes dir)]
    (doseq [[name url] existing-remotes
            :let [url-in-file (get-in data [:remotes name])]]
      (if (= url url-in-file)
        (git "fetch" name :dir dir)
        (throw (ex-info "Mismatching remotes urls!" {:remote-name name,
                                                     :in-repo url,
                                                     :in-file url-in-file}))))
    (doseq [[name url] (remove (comp existing-remotes key) (:remotes data))]
      (git "remote" "add" name url :dir dir)
      (git "fetch" name :dir dir))))

(defn sync-to-local*
  [data cfg]
  (pdoseq [[repo-name data] (:repos data)]
    (sync-to-local** (fs/file (:dir cfg) repo-name) data)))

(defn sync-to-local
  [{:keys [file], :as cfg}]
  (sync-to-local* (-> file slurp read-string) cfg))


(defn status*
  [registered-data local-data]
  (let [keyset (comp set keys)
        [only-registered only-local both]
        (diff (-> registered-data :repos keyset)
              (-> local-data      :repos keyset))

        differences
        (for [repo-name both
              :let [local-branches (get-in local-data [:repos repo-name :branches])
                    registered-branches (get-in registered-data [:repos repo-name :branches])

                    [only-registered only-local both]
                    (diff (keyset registered-branches) (keyset local-branches))

                    different-branches (->> both
                                            (filter #(not= (local-branches %)
                                                           (registered-branches %))))]]
          {:repo-name repo-name,
           :only-local only-local,
           :only-registered only-registered,
           :different different-branches})

        [okay differences] ((juxt filter remove)
                            #(->> % (vals) (remove string?) (apply concat) (empty?))
                            differences)]
    (doseq [repo-name (sort only-registered)]
      (println "Uncloned repo:" repo-name))
    (doseq [repo-name (sort only-local)]
      (println "New (unregistered) repo:" repo-name))
    (doseq [{:keys [repo-name only-local only-registered different]}
            differences]
      (printf "Differences in %s:\n" repo-name)
      (when (seq only-local)
        (println "Branches only in the local repo:" (-> only-local sort pr-str)))
      (when (seq only-registered)
        (println "Branches missing from the local repo:" (-> only-registered sort pr-str)))
      (when (seq different)
        (println "Branches at different commits:" (-> different sort pr-str))))
    (when (seq both)
      (println (count okay) "repos are probably fine."))))

(defn status
  [{:keys [file], :as cfg}]
  (status* (-> file slurp read-string)
           (read-repo-directory-data cfg)))

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
