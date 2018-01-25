(ns farg.util-test
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [clojure.test :refer :all]
            [clojure.pprint :refer :all]
            [farg.util :as util :refer [with-rng-seed almost= sample-normal]]))

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
