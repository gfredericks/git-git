(ns com.gfredericks.git-git.util
  (:require [clojure.walk :refer [postwalk]])
  (:import [java.util.concurrent Executors ExecutorService]))

(defn canonize
  [ob]
  (postwalk
   (fn [ob] (if (map? ob) (into (sorted-map) ob) ob))
   ob))

(defn call-for-each
  [func coll]
  (let [^ExecutorService tp (Executors/newFixedThreadPool 50)]
    (->> coll
         (map (fn [x] (bound-fn [] (func x))))
         (.invokeAll tp)
         (map deref)
         (dorun))
    (.shutdown tp)))

(defmacro pdoseq
  [bindings & body]
  (let [[name coll] bindings]
    `(call-for-each (fn [~name] ~@body) ~coll)))
