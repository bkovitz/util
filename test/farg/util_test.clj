(ns farg.util-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.test :refer :all]
            [clojure.pprint :refer :all]
            [farg.util :as util :refer
              [dd with-rng-seed almost= sample-normal defopts piecewise-linear
               next-id]]))

(defn sample-normals []
  (repeatedly 10 #(sample-normal)))

(deftest test-sample-normal  ;; only a smoke test
  (sample-normals))

(deftest test-with-rng
  (let [from-seed #(with-rng-seed %
                     (-> (sample-normals) set))
        from-1a (from-seed 1)
        from-1b (from-seed 1)
        from-2a (from-seed 2)
        from-2b (from-seed 2)
        from-nil1 (from-seed nil)
        from-nil2 (from-seed nil)]
    (is (= from-1a from-1b))
    (is (= from-2a from-2b))
    (is (not= from-1a from-2a))
    (is (not= from-nil1 from-1a))
    (is (not= from-nil1 from-nil2))))

(deftest test-stretch-unit-interval
  (is (= -1.0 (util/stretch-unit-interval -2.0 0.0 0.5))))

(deftest test-choose-one
  (is (= (with-rng-seed 1
           (vec (repeatedly 10 #(util/choose-one :a :b))))
         [:b :a :a :a :b :a :b :b :b :b]))) ;relative to seed 1

(defopts let-test-opts
  {:keys [iterations param2]
   :or {iterations 10}}
  derived (* 2 iterations))

(defn f-opts [& opts]
  (let-test-opts opts
    [iterations param2 derived]))

(defn f-pass-opts [& opts]
  (apply f-opts opts))

(defn f-pass-opts2 [& opts]
  (apply f-pass-opts opts))

(defn f-pass-opts3 [& opts]
  (apply f-pass-opts opts))

(defn f-pass-opts-the-good-way [& opts]
  (let-test-opts opts
    (f-opts opts))) ;opts has been rebound to a map

(defopts let-no-or {:keys [a b c] :or {a 22}})

(deftest test-defopts
  (is (= [10 nil 20] (f-opts)))
  (is (= [6 :this 12] (f-opts :param2 :this, :iterations 6)))
  (is (= [6 :this 12] (f-opts {:iterations 6, :param2 :this})))

  (is (= [10 nil 20] (f-pass-opts)))
  (is (= [6 :this 12] (f-pass-opts :param2 :this, :iterations 6)))
  (is (= [6 :this 12] (f-pass-opts {:iterations 6, :param2 :this})))

  (is (= [10 nil 20] (f-pass-opts2)))
  (is (= [6 :this 12] (f-pass-opts2 :param2 :this, :iterations 6)))
  (is (= [6 :this 12] (f-pass-opts2 {:iterations 6, :param2 :this})))

  (is (= [10 nil 20] (f-pass-opts3)))
  (is (= [6 :this 12] (f-pass-opts3 :param2 :this, :iterations 6)))
  (is (= [6 :this 12] (f-pass-opts3 {:iterations 6, :param2 :this})))

  (is (= [10 nil 20] (f-pass-opts-the-good-way)))
  (is (= [6 :this 12] (f-pass-opts-the-good-way :param2 :this, :iterations 6)))
  (is (= [6 :this 12]
         (f-pass-opts-the-good-way {:iterations 6, :param2 :this})))

  (is (= [1 2 nil {:a 1 :b 2}]
         (let [options [:b 2 :a 1]]
           (let-no-or options
             [a b c options]))))
  
  (let [opts {:new-param 'new}]
    (let-test-opts opts
      (is (= 'new (:new-param opts)))
      ;(is (= 'new new-param)) ;If it's not defined in the defopts, it won't
                               ;get bound.
      (is (= {:new-param 'new :iterations 10 :derived 20} opts)))))

(deftest test-weighted-choice-by
  (with-rng-seed 1
    (let [choices [{:item :a, :weight 5.0}
                   {:item :b, :weight 3.5}
                   {:item :c, :weight 1.5}]
          freqs (->> (repeatedly 250 #(util/weighted-choice-by :weight choices))
                     (map :item)
                     frequencies
                     util/normalize-vals)
          a= (partial almost= 0.1)]
      (is (a= 0.50 (freqs :a)))
      (is (a= 0.35 (freqs :b)))
      (is (a= 0.15 (freqs :c))))))

(deftest test-piecewise-linear
  (let [f (piecewise-linear 0.0 1.0
                            1.0 1.0
                            2.0 2.0
                            3.0 4.0)]
    (is (= (f -1.0) 1.0))
    (is (= (f 0.0) 1.0))
    (is (= (f 0.6) 1.0))
    (is (= (f 1.0) 1.0))
    (is (= (f 1.5) 1.5))
    (is (= (f 2.0) 2.0))
    (is (= (f 2.5) 3.0))
    (is (= (f 3.0) 4.0))
    (is (= (f 4.0) 6.0))))

(deftest test-piecewise-linear-double-xs
  (let [f (piecewise-linear 0.0 0.0
                            0.1 1.0
                            0.1 2.0
                            0.2 3.0)]
    (is (= -10.0 (f -1.0)))
    (is (=   0.5 (f  0.05)))
    (is (=   1.0 (f  0.1)))
    (is (=   2.5 (f  0.15)))))

(deftest test-piecewise-linear-double-xs-first
  (let [f (piecewise-linear 0.0 0.0
                            0.0 0.1
                            1.0 0.2)]
    (is (almost= 0.0 (f 0.0)))
    (is (almost= 0.15 (f 0.5)))))

(deftest test-piecewise-linear-no-xs
  (let [f (piecewise-linear)]
    (is (= 42.4 (f 42.4)))))

(deftest test-piecewise-linear-one-x
  (let [f (piecewise-linear 1.0 3.0)]
    (is (= 2.0 (f 0.0)))
    (is (= 3.0 (f 1.0)))))

(deftest test-piecewise-linear-cliff-at-end
  (let [f (piecewise-linear 0.0 0.0, 10.0 1.0, 10.0 0.0)]
    (is (= 0.5 (f 5.0)))
    (is (= 1.0 (f 10.0)))
    (is (= 0.0 (f 10.00000001)))
    (is (= 0.0 (f 11.00000001)))))

(deftest test-stems-and-suffixes
  (let [stem-map {}
        [stem-map plus] (next-id stem-map :plus)
        [stem-map plus2] (next-id stem-map :plus)
        [stem-map source10] (next-id stem-map :source10)
        [stem-map source11] (next-id stem-map :source11)
        [stem-map source10a] (next-id stem-map :source10)
        [stem-map plus3] (next-id stem-map :plus)]
    (is (= :plus plus))
    (is (= :plus002 plus2))
    (is (= :source10 source10))
    (is (= :source11 source11))
    (is (= :source10a source10a))
    (is (= :plus003 plus3)))
    (is (= :source10aa
           (->> (iterate (fn [[stem-map id]]
                           (next-id stem-map :source10))
                         [{} nil])
                (map second)
                (drop 28)
                first))))
