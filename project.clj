(defproject com.gfredericks/git-git "0.1.0-SNAPSHOT"
  :description "Managing your swarm of git repositories."
  :url "https://github.com/fredericksgary/git-git"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[environ "0.4.0"]
                 [me.raynes/conch "0.6.0"]
                 [me.raynes/fs "1.4.5"]
                 [org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.cli "0.2.4"]
                 [org.clojure/core.typed "0.2.19"]
                 [robert/hooke "1.3.0"]]


  ;; building
  :aot :all
  :main com.gfredericks.git-git
  :uberjar-name "git-git.jar")
