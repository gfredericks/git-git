(ns com.gfredericks.git-git.util
  (:require [clojure.walk :refer [postwalk]]))

(defn canonize
  [ob]
  (postwalk
   (fn [ob] (if (map? ob) (into (sorted-map) ob) ob))
   ob))
