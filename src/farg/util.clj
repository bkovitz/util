(ns farg.util
  "Generally useful utility functions from the FARG library."
  (:refer-clojure :exclude [rand rand-int cond])
  (:require [better-cond.core :refer [cond]]
            [farg.with-state :refer [with-state]]
            [clojure.core.async :as async :refer [<! <!! >! >!!]]
            [clojure.core.matrix.random]
            [clojure.core.strint :refer [<<]]
            [clojure.math.numeric-tower :as math]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.tools.trace :refer [deftrace] :as trace]
            [com.rpl.specter :as S]
            [incanter.stats :as stats]))

;; Printing

#_(defmacro with-*out* [filename & body]
  `(with-open [file# (clojure.java.io/writer ~filename)]
     (binding [*out* file#]
       ~@body)))

; Let's try that again
(defmacro with-*out* [file- & body]
  `(binding [*out* ~file-]
     ~@body))

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
      `(let [v# ~expr
             v# (if (seq? v#) (doall v#) v#)]
         (println '~expr "=>" (pr-str v#)))))

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

;;; defopts

(defn get-opts
  "Helper for defopts. Extracts options from opts argument, whether they're
  keyword arguments, a map, or the same after having been passed from the
  & opts argument of another function."
  [opts]
  (loop [opts opts]
    (if (= 1 (count opts))
        (if (map? (first opts))
            (apply concat (first opts))
            (recur (first opts)))
        (seq opts))))

(defn as-map
  "Coerces x to a map."
  [x]
  (cond
    (map? x)
      x
    (seqable? x)
      (cond
        (empty? x)
          {}
        :let [c (count x)]
        (= 1 c)
          (recur (first x))
        (even? c)
          (apply hash-map x)
        (throw (IllegalArgumentException.
                 (str "Can't coerce " x " to map: odd number of elements."))))
    (throw (IllegalArgumentException. (str "Can't coerce " x " to map.")))))

(defmacro defopts
  "Defines optional arguments in a form that can be passed as keyword arguments
  or as a map. Works by defining a 'let' macro. See the unit test."
  [macro-name destructure-map & binding-pairs]
  (let [binding-pairs (vec binding-pairs)
        dmap-keys (:keys destructure-map)
        or-map (:or destructure-map)
        overrides (->> binding-pairs (take-nth 2) (concat (keys or-map)) vec)
        kws (->> overrides (map keyword) vec)
        opts-expr (vec (map vector kws overrides))]
    `(defmacro ~macro-name [opts# & body#]
      ;TODO Throw exception if opts is not a symbol
      (let [dmap# {:keys '~dmap-keys :or '~or-map :as opts#}]
        `(let [~opts# (as-map ~opts#)
               ~dmap# ~opts#]
           (let ~'~binding-pairs
             (let [~opts# (into ~opts# ~'~opts-expr)]
               ~@body#)))))))

;;; Randomness

(defn new-seed []
  (. System nanoTime))

(def ^:dynamic *rng-seed* (new-seed))
  ;; The seed that *rng* started with; not necessarily its current seed.

(defn make-rng [seed]
  (java.util.Random. seed))

(def ^:dynamic *rng* (make-rng *rng-seed*))

(defmacro with-rng-seed
  "n is seed for random-number generator to install at *rng* for duration of
  body. If n is nil, makes a new seed based on the time.  Either way, stores
  the seed at *rng-seed* If n is :continue, leaves *rng* and *rng-seed*
  unchanged."
  [n & body]
  `(letfn [(do-body# [] ~@body)]
     (let [n# ~n]
       (cond
         (= :continue n#)
           (do-body#)
         :let [n# (if (nil? n#) (new-seed) n#)]
         (binding [*rng-seed* n#, *rng* (make-rng n#)]
           (do-body#))))))

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

(defn choose-from [coll]
  (nth (seq coll) (rand-int (count coll)) nil))

(def choose choose-from)

;TODO Replace all calls to this with calls to choose-expr
(defmacro choose-one [& choices]
  (if (empty? choices)
    nil
    `(case (rand-int ~(count choices))
       ~@(mapcat #(vector %1 %2) (range) choices))))

(defmacro choose-expr [& choices]
  (if (empty? choices)
    nil
    `(case (rand-int ~(count choices))
       ~@(mapcat #(vector %1 %2) (range) choices))))

; By Christophe Grand (modified by Ben Kovitz to force v to be a vector)
; https://groups.google.com/d/msg/clojure/Kj0b_YhXcos/UahpU7m3iJcJ
(defn lazy-shuffle [v]
  (let [v (vec v)]
    (lazy-seq
      (if (seq v)
          (let [idx (rand-int (count v))]
            (cons (nth v idx)
              (lazy-shuffle (pop (assoc v idx (peek v))))))))))

(defn- weight-choice-i*
  "Returns randomly chosen index, weighted by the weights. Assumes that there
  is at least one weight and that there are no zero weights."
  [weights]
  (let [total (reduce + weights)
        r (rand total)
        n (count weights)]
    (loop [i 0, a 0.0]
      (when (< i n)
        (let [a (+ a (nth weights i))]
          (if (<= r a)
            i
            (recur (inc i) a)))))))

(defn weighted-lazy-shuffle
  "'choices' is a sequence of [item weight]. Returns a lazy sequence of the
  items, each consecutive item chosen randomly from the remaining, weighted
  by 'weight'."
  [choices]
  (let [choices (->> choices (filter #(> (second %) 0.0)) vec)]
    (lazy-seq
      (when (not (empty? choices))
        (let [i (weight-choice-i* (map second choices))]
          (cons (first (nth choices i))
                (weighted-lazy-shuffle
                  (pop (assoc choices i (peek choices))))))))))

;TODO UT
(defn lazy-deterministic-shuffled-indices [seed ub]
  (let [rng (java.util.Random. seed)]
    (letfn [(next-int [] (.nextInt rng ub))
            (lazy [so-far]
              (lazy-seq
                (when (< (count so-far) ub)
                  (let [i (->> (repeatedly next-int)
                               (filter #(not (contains? so-far %)))
                               (first))]
                    (cons i (lazy (conj so-far i)))))))]
      (lazy #{}))))

;TODO UT
(defn make-flatten-coords
  "Returns a function that maps an integer index to a multidimensional
  coordinate.

  row: A vector of all the numbers in a single dimension.

  dimension: The number of dimensions in the coordinates."
  [row dimension]
  (let [row-size (count row)]
    (fn [index]
      (loop [index index, coords [], n dimension]
        (cond
          (zero? n) coords
          :let [i (rem index row-size)
                index (quot index row-size)]
          (recur index (conj coords (get row i)) (dec n)))))))

(defn choose-with-replacement
  [n coll]
  (take n (repeatedly #(choose coll))))

;TODO UT
(defn choose-without-replacement
  "Chooses n items randomly from coll, with uniform probability."
  [n coll]
  (take n (lazy-shuffle coll)))

(defn weighted-choice
  "'choices' must be a seq of two-element vectors each in the format
  [item weight]. Returns a randomly chosen 'item' with probability weighted by
  'weight'. Returns nil if 'choices' is empty or the weights sum to zero."
  [choices]
  (let [choices (->> choices (filter #(> (second %) 0.0)) vec)
        total (reduce + (map second choices))
        r (rand total)
        n (count choices)]
    (loop [i 0, a 0.0]
      (when (< i n)
        (let [a (+ a (second (choices i)))]
          (if (<= r a)
              (first (choices i))
              (recur (inc i) a)))))))

(defn weighted-choice-by
  "Like weighted-choice but caller provides a weight function (such as a key)
  to indicate the weight of each choice."
  [weight-f choices]
  (let [weights (map weight-f choices)
        total (reduce + weights)
        r (rand total)]
    (loop [sum 0.0, choices choices, weights weights]
      (let [sum (+ sum (first weights))]
        (if (<= r sum)
            (first choices)
            (recur sum (next choices) (next weights)))))))

(defn weighted-choice-by-without-replacing
  "Like weighted-choice-by except ..."
  [weight-f n choices]
  (if (<= (count choices) n)
      choices
      (loop [choices (set choices), result []]
        (if (= n (count result))
            result
            (let [chosen (weighted-choice-by weight-f choices)]
              (recur (disj choices chosen) (conj result chosen)))))))

(defn stretch-unit-interval
  "x must be in unit interval: [0.0, 1.0]. Returns x mapped to corresponding
  point in interval [lb, ub]."
  [lb ub x]
  (+ lb (* x (- ub lb))))

(defn sample-normal [& {:keys [rng mean sd] :or {rng *rng*}}]
  (with-state [N (first (clojure.core.matrix.random/sample-normal 1 rng))]
    (when (some? sd)
      (* sd))
    (when (some? mean)
      (+ mean))))

(def circle-interval [0.0 (* 2.0 Math/PI)])

(defn sample-uniform [[lb ub] & {:keys [rng] :or {rng *rng*}}]
  (->> (clojure.core.matrix.random/sample-uniform 1 rng)
       first
       (stretch-unit-interval lb ub)))

(defn sample-poisson [λ]
  (letfn [(cdf [k] (stats/cdf-poisson k :lambda λ))]
    (let [r (rand)]
      (loop [k 0]
        (if (<= r (cdf k))
          k
          (recur (inc k)))))))

;;; Miscellaneous functions

(defn =sets [xs ys]
  (= (set xs) (set ys)))

(defn =msets
  "Equal as multisets?"
  [xs ys]
  (and (= (count xs) (count ys))
       (= (set xs) (set ys))))

;TODO UT
;By 象嘉道, from https://stackoverflow.com/a/32405094/1393162
(defn find-first [pred coll]
  (reduce #(when (pred %2) (reduced %2)) nil coll))

(defn mapvals [f m]
  (zipmap (keys m) (map f (vals m))))

(defn remove=
  "Returns lazy seq of all elements in coll that are not equal to v."
  [v coll]
  (remove #(= v %) coll))

(defn almost=
 ([a b]
  (almost= 0.001 a b))
 ([tolerance a b]
  (and (number? a)
       (number? b)
       (< (math/abs (- a b)) tolerance))))

(defn vector-contains? [v x]
  (some #(= % x) v))

(defn safe-derive [h child parent]
  (if (isa? h child parent)
    h
    (derive h child parent)))

(defn inheritance-seq
  "Returns lazy seq of tag and ancestors of tag sorted topologically so that
  items higher in the hierarchy always appear before items that derive from
  them. Within one level, though, the sequence is arbitrary (though
  deterministic).  tag always appears last in the result."
  [hierarchy tag]
  (distinct
    (loop [to-go (seq (parents hierarchy tag)), result (into (list tag) to-go)]
      (cond
        (empty? to-go)
          result
        :let [tag (first to-go), ps (parents hierarchy tag)]
        (recur (concat (rest to-go) ps)
               (into result ps))))))

;TODO UT
(defn safe-min
  "Like clojure.core/min but returns 0.0 if given no arguments and ignores
  nil arguments."
  [& args]
  (cond
    :let [args (filter some? args)]
    (empty? args)
      0.0
    (apply min args)))

;TODO UT
(defn safe-max
  "Like clojure.core/min but returns 0.0 if given no arguments and ignores
  nil arguments."
  [& args]
  (cond
    :let [args (filter some? args)]
    (empty? args)
      0.0
    (apply max args)))

(defn clamp [[lower-bound upper-bound] x]
  (cond
    (< x lower-bound)
      lower-bound
    (> x upper-bound)
      upper-bound
    :else
      x))

(defn clamp-unit [x] (clamp [0.0 1.0] x))

(defn mround [x]
  (-> x (* 1000) (math/round) (/ 1000.0)))

(defn mround-all [x]
  (S/transform (S/walker float?) mround x))

(defn mroundstr-all
 ([x]
  (mroundstr-all "%4.3f" x))
 ([fmt x]
  (S/transform (S/codewalker float?) #(format fmt %) x)))

(defn midp [x0 x1]
  (/ (+ x0 x1) 2))

(defn midpoint [p1 p2]
  (if (vector? p1)
    (-> (map midp p1 p2) vec)
    (midp p1 p2)))

(defn distance [[x0 y0] [x1 y1]]
  (Math/sqrt (+ (Math/pow (- x1 x0) 2.0) (Math/pow (- y1 y0) 2.0))))

(defn polar->rectangular
 ([r θ]
  [(* r (Math/cos θ)) (* r (Math/sin θ))])
 ([[r θ]]
  (polar->rectangular r θ)))

(defn average [coll]
  (if (empty? coll)
      0.0
      (/ (reduce + 0.0 coll)
         (count coll))))

(defn average-or-nil [coll]
  (if (empty? coll)
      nil
      (/ (reduce + 0.0 coll)
         (count coll))))

(defn make-sigmoid-fn
  "Returns a logistic function with given center and range, with given
  slope at (x-center, (/ (+ y-max y-min) 2))."
  [x-center y-min y-max slope]
  (let [y-scale (- y-max y-min)
        y-center (/ (+ y-max y-min)
                    2)
        y-offset (- y-center (/ y-scale 2))]
    (fn [x]
      (+ (/ y-scale
            (+ 1.0 (Math/exp (* slope (- x-center x)))))
         y-offset))))

#_(defn normalize
  [target-sum coll]
  (if (empty? coll)
    coll
    (let [factor (/ target-sum (reduce + coll))]
      (map #(* factor %) coll))))

#_(defn normalize-vals
  "Scales vals of map m so they sum to 1.0."
  ;TODO Should call normalize
  [m]
  (let [total (->> (vals m) (map float) (reduce +))]
    (if (zero? total)
        m
        (zipmap (keys m) (->> (vals m) (map #(/ % total)))))))

(defn normalize
  [target-sum coll]
  (cond
    (empty? coll)
      coll
    :let [total (reduce + coll)]
    (zero? total)
      coll
    :let [scaling-factor (/ target-sum total)]
    (= 1.0 scaling-factor)
      coll
    (map #(* scaling-factor %) coll)))

;TODO UT arity 2
(defn normalize-vals
 ([m]
  (normalize-vals 1.0 m))
 ([target-sum m]
  (zipmap (keys m) (normalize target-sum (vals m)))))

;TODO UT
(defn scale-down
  [target-sum coll]
  (cond
    (empty? coll)
      coll
    :let [total (reduce + coll)]
    (or (<= total target-sum) (zero? total))
      coll
    :let [scaling-factor (/ target-sum total)]
    (map #(* scaling-factor %) coll)))

;TODO UT
(defn scale-down-vals
 ([m]
  (scale-down-vals 1.0 m))
 ([target-sum m]
  (zipmap (keys m) (scale-down target-sum (vals m)))))

;NEXT Write this.
;(defn bin-counts
;  [nbins f coll]

(def empty-dstats ^{:type ::dstats} {:type ::dstats})

(defn dstats
  "Descriptive statistics for numbers in coll."
  [coll]
  (if (empty? coll)
    (assoc empty-dstats :n 0 :mean 0.0 :sd 0.0 :median 0.0 :min 0.0 :max 0.0)
    (assoc empty-dstats :n (count coll) :mean (stats/mean coll)
                        :sd (stats/sd coll) :median (stats/median coll)
                        :min (apply min coll) :max (apply max coll))))

(defn nf
  "A number, converted to a string and nicely formatted for most purposes."
  [x]
  (cond
    (nil? x) "nil"
    (integer? x) (str x)
    (float? x) (format "%1.3g" x)))

(defn non-lazy [x]
  (if (coll? x) (doall x) x))

(defn map-str
  "Like (str m) except the keys are sorted and all floating-point numbers are
  formatted as strings with three digits after the decimal point."
  [m]
  (str \{
       (clojure.string/join ", "
         (->> (mroundstr-all m)
              (sort-by key)
              (map #(print-str (key %) (val %)))))
       \}))

(defmethod print-method ::dstats [v ^java.io.Writer w]
  (let [{:keys [n mean sd median min max]} v]
    (.write w (<< "n=~{n} m=~(nf mean) sd=~(nf sd) med=~(nf median)"
                  " min=~(nf min) max=~(nf max)"))))

(defn piecewise-linear
  "Returns a piecewise-linear function that passes through the given points."
  [& points]
  ;TODO Ensure that xs increase monotonically.
  (let [points (vec (partition 2 points))]
    (cond
      (zero? (count points))
        identity
      (= 1 (count points))
        (let [[[x0 y0]] points
              offset (- y0 x0)]
          (fn [x] (+ x offset)))
      :else
        (fn [x]
          (loop [[[x0 y0] & more] points]
            (let [[[x1 y1]] more]
              (cond
                (<= x x1)
                  (let [Δx (- x1 x0), Δy (- y1 y0)
                        m (if (zero? Δx) 0.0 (/ Δy Δx))]
                    (+ y0 (* m (- x x0))))
                (empty? (rest more))
                  (let [Δx (- x1 x0), Δy (- y1 y0)]
                    (if (zero? Δx)
                        y1
                        (let [m (/ Δy Δx)]
                          (+ y0 (* m (- x x0))))))
                :else
                  (recur more))))))))

;; Keyword stems and suffixes

(defn namestr [x]
  (if (or (keyword? x) (symbol? x))
      (name x)
      (if (integer? x)
        (format "%03d" x)
        (str x))))

(defn make-id [stem suffix]
  (cond
    (keyword? stem)
      (keyword (namespace stem) (str (namestr stem) (namestr suffix)))
    (symbol? stem)
      (symbol (namespace stem) (str (namestr stem) (namestr suffix)))
    (number? stem)
      (if (empty? suffix)
        stem
        (str stem suffix))
    (str (namestr stem) (namestr suffix))))

(defn bump-letter-suffix [suffix]
  (apply str
    (loop [suffix (seq suffix), post-suffix ()]
      (cond
        (empty? suffix)
          (cons \a post-suffix)
        (= \z (last suffix))
          (recur (butlast suffix) (cons \a post-suffix))
        :else
          `(~@(butlast suffix) 
            ~(-> (last suffix) int inc char)
            ~@post-suffix)))))

(defn bump-number-suffix [suffix]
  (if (= "" suffix) 2 (inc suffix)))

(defn bump-suffix [stem suffix]
  (let [last-char-of-stem (last (namestr stem))]
    (if (or (nil? last-char-of-stem) (Character/isDigit last-char-of-stem))
        (bump-letter-suffix suffix)
        (bump-number-suffix suffix))))
      
(defn next-id
  "Returns new stem-map and id. Stems that end in a number get alphabetic
  suffixes for successive ids. All other stems get numeric suffixes for
  successive ids. The first id is always the stem with no suffix."
  [stem-map stem]
  (let [stem-map (update stem-map stem
                         #(if (nil? %) "" (bump-suffix stem %)))]
    [stem-map (make-id stem (get stem-map stem))]))

;; Files

(defn rm-recursively
  "Based on https://gist.github.com/edw/5128978#gistcomment-2232844
  by ignorabilis. f can be a filename or a file object, and can refer
  to a directory or a file. Removes f and any files and subdirectories
  that it contains."
 ([f]
  (rm-recursively f :silently))
 ([f silently?]
  (letfn [(rm-f [f]
            (when (.isDirectory f)
              (doseq [child-f (.listFiles f)]
                (rm-f child-f)))
            (io/delete-file f silently?))]
    (rm-f (io/file f)))))
