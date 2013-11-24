(ns com.gfredericks.git-git-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer :all]
            [com.gfredericks.git-git.actions :as actions]
            [com.gfredericks.git-git :refer [sync-to-local update-from-local]
                                     :as git-git]
            [me.raynes.fs :as fs]
            [me.raynes.conch :refer [programs with-programs let-programs]]
            [robert.hooke :refer [add-hook]]))

(defn update-final-map
  [func coll]
  (if (map? (last coll))
    (concat (butlast coll) [(func (last coll))])
    (concat coll [(func nil)])))

(defn conch-dir-hook
  "Sets the :dir of a conch fn to fs/*cwd*."
  [orig-conch-fn & args]
  (let [args' (update-final-map (fn [m] (merge {:dir fs/*cwd*} m)) args)]
    (apply orig-conch-fn args')))

(defn tempdir-fixture
  [test]
  (let [dir (fs/temp-dir "git-git-test-")]
    (try (fs/with-cwd dir (test))
         (finally
           (fs/delete-dir dir)))))

(defn quiet-fixture
  [test]
  (binding [com.gfredericks.git-git.io/*quiet?* true]
    (test)))

(use-fixtures :each tempdir-fixture quiet-fixture)

(programs git cat)
(add-hook #'git conch-dir-hook)
(add-hook #'cat conch-dir-hook)

(defn create-git-repo
  ([dir] (create-git-repo dir (str "/not-a-real-git-repo/" dir ".git")))
  ([dir origin]
     (let [dir (fs/file fs/*cwd* dir)]
       (fs/mkdir dir)
       (git "init" {:dir dir})
       (git "remote" "add" "origin" origin {:dir dir}))))

(deftest update-from-local-test
  (is (empty? (fs/list-dir fs/*cwd*)))
  (create-git-repo "foo")
  (is (= 1 (count (fs/list-dir fs/*cwd*))))
  (let [file (fs/temp-file "git-git-test-config-")]
    (assert fs/*cwd*)
    (update-from-local {:dir fs/*cwd*
                        :file file})
    (let [{repos :repos} (-> file slurp edn/read-string)]
      (is (= 1 (count repos)))
      (is (= (get repos "foo")
             {:remotes {"origin" "/not-a-real-git-repo/foo.git"}
              :branches {}})))))

(deftest update-from-local-with-dotted-repo-name-test
  (create-git-repo "core.logic")
  (let [file (fs/temp-file "git-git-test-config-")]
    (assert fs/*cwd*)
    (update-from-local {:dir fs/*cwd*
                        :file file})
    (let [{repos :repos} (-> file slurp edn/read-string)]
      (is (= 1 (count repos)))
      (is (= (get repos "core.logic")
             {:remotes {"origin" "/not-a-real-git-repo/core.logic.git"}
              :branches {}})))))

(defn current-master-sha
  []
  (.trim (cat ".git/refs/heads/master")))

(deftest sync-to-local-test
  (let [tmpdir (fs/temp-dir "github")
        master-sha (fs/with-cwd tmpdir
                     (create-git-repo "fazzle")
                     (fs/with-cwd (fs/file tmpdir "fazzle")
                       (spit (fs/file "poopsticks.txt") "this is my code")
                       (git "add" "poopsticks.txt")
                       (git "commit" "-a" "-m" "Making a commit in my test")
                       (current-master-sha)))

        data {:repos
              {"foobert"
               {:remotes
                {"origin" (str tmpdir "/" "fazzle")}
                :branches
                {"master" master-sha}}}}
        file (fs/temp-file "haw-whateuvr")]
    (spit file (pr-str data))
    (sync-to-local
     {:file file
      :dir fs/*cwd*})
    (is (fs/exists? "foobert"))
    (let [f (fs/file "foobert" "poopsticks.txt")]
      (is (fs/exists? f))
      (is (= "this is my code" (slurp f))))))

(deftest roundtrip-test
  (let [tmpdir (fs/temp-dir "github")
        master-sha (fs/with-cwd tmpdir
                     (create-git-repo "fazzle")
                     (fs/with-cwd (fs/file tmpdir "fazzle")
                       (spit (fs/file "poopsticks.txt") "this is my code")
                       (git "add" "poopsticks.txt")
                       (git "commit" "-a" "-m" "Making a commit in my test")
                       (current-master-sha)))]
    (let [data {:repos
                {"foobert"
                 {:remotes
                  {"origin" (str tmpdir "/" "fazzle")
                   "other-remote" (str tmpdir "/" "fazzle")}
                  :branches
                  {"master" master-sha}}}}
          cfg {:dir fs/*cwd*}]
      (with-redefs [actions/read-registry-file (constantly data)]
        (git-git/sync-to-local cfg))
      (let [reg-file (fs/temp-file "git-git-test-")]
        (git-git/update-from-local (assoc cfg :file reg-file))
        (is (= data (-> reg-file slurp read-string)))))))
