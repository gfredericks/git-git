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
  (let [^ExecutorService tp (Executors/newFixedThreadPool 50)
        fs (doall (map (fn [x]
                         (let [^Runnable f #(func x)]
                           (.submit tp f)))
                       coll))]
    (doseq [f fs] (deref f))
    (.shutdown tp)))

(defmacro pdoseq
  [bindings & body]
  (let [[name coll] bindings]
    `(call-for-each (fn [~name] ~@body) ~coll)))
