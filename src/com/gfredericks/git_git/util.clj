(ns com.gfredericks.git-git.util
  (:refer-clojure :exclude [assoc-in])
  (:require [clojure.core.typed :refer :all]
            [clojure.walk :refer [postwalk]])
  (:import [java.util.concurrent Executors ExecutorService]))

(defn canonize
  [ob]
  (postwalk
   (fn [ob] (if (map? ob) (into (sorted-map) ob) ob))
   ob))

(ann call-for-each
     (All [x] [[x -> nil] (Seqable x) -> nil]))
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

(defmacro assoc-in
  "Like clojure.core/assoc-in but is a macro so it can
  type-check. Requires the keypath to be a literal."
  [m ks v]
  (if (= 1 (count ks))
    `(assoc ~m ~(first ks) ~v)
    `(let [m# ~m
           k# ~(first ks)]
       (assoc m#
              k#
              (assoc-in (get m# k#) ~(rest ks) ~v)))))
