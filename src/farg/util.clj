(ns farg.util
  "Generally useful utility functions from the FARG library."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.core.async :as async :refer [<! <!! >! >!!]]
            [clojure.core.matrix.random]
            [clojure.math.numeric-tower :as math]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.tools.trace :refer [deftrace] :as trace]))

;; Debugging aids

(defmacro dde
  "\"Display for debugging.\" Surround an expression that you want to print
  with dd, and it will print it and pass the value through. Precede with
  other expressions and it will print those, too, preceded by the expression.
  A string literal will print as itself on a line by itself, to help you
  find it in a stream of output."
  [expr & more]
  (cond
    (and (nil? expr) (nil? more))
      `(println "dde: no args")
    (nil? more)
      `(let [result# ~expr]
         (print '~expr "=> ")
         (prn result#)
         result#)
    :else
      (if (string? expr)
          `(do
             (prn ~expr)
             (dde ~@more))
          `(let [v# ~expr
                v# (if (coll? v#) (doall v#) v#)]
             (prn '~expr v#)
             (dde ~@more)))))

(defn dd1- [expr]
  (cond
    (= \newline expr)
      `(println)
    (string? expr)
      `(prn ~expr)
    :else
      `(println '~expr "=>" (str ~expr))))

(defn- dd- [exprs]
  (cond
    (empty? exprs)
      nil
    (empty? (rest exprs))
      `(do
         ~(dd1- (first exprs))
         ~(first exprs))
    :else
      `(do
         ~(dd1- (first exprs))
         ~(dd- (rest exprs)))))

(defmacro dd
  "\"Display for debugging.\" Prints each expr and what it evaluates to.
  Returns the value of the last expr. Does not double-print string literals."
  [& exprs]
  `(binding [*out* *err*]
    ~(dd- exprs)))

(defn trace-cond- [clauses]
  (cond
    (empty? clauses) `nil
  :else
    (let [[condition result & more-clauses] clauses]
      `(let [holds?# ~condition]
         (println '~condition "=>" (boolean holds?#))
         (if holds?#
             (let [got# ~result]
               (println \space '~result "=>" got#)
               got#)
             ~(trace-cond- more-clauses))))))

(defmacro trace-cond [& clauses]
  (if (even? (count clauses))
      (trace-cond- clauses)
      (throw (IllegalArgumentException.
               "trace-cond must have an even number of arguments"))))

;;; Random numbers

(defn new-seed []
  (. System nanoTime))

(def ^:dynamic *rng-seed* (new-seed))
  ;; The seed that *rng* started with; not necessarily its current seed.

(defn make-rng [seed]
  (java.util.Random. seed))

(def ^:dynamic *rng* (make-rng *rng-seed*))

(defmacro with-rng-seed [n & body]
  `(let [n# ~n
         n# (if (nil? n#) (new-seed) n#)]
     (binding [*rng-seed* n#, *rng* (make-rng n#)]
     ~@body)))

(defn rand
 ([]
  (.nextDouble *rng*))
 ([n]
  (* n (rand)))
 ([lb ub]
  (+ (rand (- ub lb))
     lb)))

(defn rand-int
  "both lb and ub are inclusive."
 ([n]
  (int (rand n)))
 ([lb ub]
  (+ (rand-int (inc (- ub lb)))
     lb)))

; By Christophe Grand (modified by Ben Kovitz to force v to be a vector)
; https://groups.google.com/d/msg/clojure/Kj0b_YhXcos/UahpU7m3iJcJ
(defn lazy-shuffle [v]
  (let [v (vec v)]
    (lazy-seq
      (if (seq v)
          (let [idx (rand-int (count v))]
            (cons (nth v idx)
              (lazy-shuffle (pop (assoc v idx (peek v))))))))))

(defn sample-normal [& {:keys [rng] :or {rng *rng*}}]
  (first (clojure.core.matrix.random/sample-normal 1 rng)))

;;; Miscellaneous functions

(defn almost=
 ([a b]
  (almost= 0.001 a b))
 ([tolerance a b]
  (and (number? a)
       (number? b)
       (< (math/abs (- a b)) tolerance))))
