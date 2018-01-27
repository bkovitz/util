(ns farg.util-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.test :refer :all]
            [clojure.pprint :refer :all]
            [farg.util :as util :refer
              [with-rng-seed almost= sample-normal defopts]]))

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
             [a b c options])))))
