(ns br.com.souenzzo.cljc-core
  (:refer-clojure :only [defn first next defmacro list cons ex-info let
                         reduced reduced? fn volatile! vreset! sequence =
                         reduce persistent! transient apply assoc  nil? seq assoc! conj get])
  (:require [clojure.core :as c]))

(defn second
  [x]
  (first (next x)))

(defmacro when
  [test & body]
  (list 'if test (cons 'do body)))

(defmacro cond
  [& clauses]
  (when clauses
        (list 'if (first clauses)
              (if (next clauses)
                (second clauses)
                (throw (ex-info "cond requires an even number of forms"
                                {:cognitect.anomalies/anomaly :cognitect.anomalies/incorrect})))
              (cons `cond (next (next clauses))))))



(defmacro or
  "Evaluates exprs one at a time, from left to right. If a form
  returns a logical true value, or returns that value and doesn't
  evaluate any of the other expressions, otherwise it returns the
  value of the last expression. (or) returns nil."
  {:added "1.0"}
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if or# or# (or ~@next)))))

(defmacro when-let
  "bindings => binding-form test

  When test is true, evaluates body with binding-form bound to the value of test"
  [bindings & body]
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (when temp#
             (let [~form temp#]
               ~@body)))))


(defn preserving-reduced
  [rf]
  #(let [ret (rf %1 %2)]
     (if (reduced? ret)
       (reduced ret)
       ret)))

(defn dedupe
  "Returns a lazy sequence removing consecutive duplicates in coll.
  Returns a transducer when no collection is provided."
  {:added "1.7"}
  ([]
   (fn [rf]
     (let [pv (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [prior @pv]
            (vreset! pv input)
            (if (= prior input)
              result
              (rf result input))))))))
  ([coll] (sequence (dedupe) coll)))


(defn cat
  "A transducer which concatenates the contents of each input, which must be a
  collection, into the reduction."
  [rf]
  (let [rrf (preserving-reduced rf)]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
       (reduce rrf result input)))))

(defn concat
  ([] nil)
  ([& xs]
   (sequence cat xs)))

(defn group-by
  "Returns a map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll."
  [f coll]
  (persistent!
    (reduce
      (fn [ret x]
        (let [k (f x)]
          (assoc! ret k (conj (get ret k []) x))))
      (transient {}) coll)))

(defn filter
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (if (pred input)
          (rf result input)
          result)))))
  ([pred coll]
   (sequence (filter pred) coll)))

(defn map
  ([pred]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
        (rf result (pred input))))))
  ([pred & colls]
   (apply sequence (map pred) colls)))

(defn every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false."
  [pred coll]
  (cond
    (nil? (seq coll)) true
    (pred (first coll)) (recur pred (next coll))
    :else false))

(defn not
  "Returns true if x is logical false, false otherwise."
  [x]
  (if x false true))

(defn identity [x] x)

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil.  One common idiom is to use a set as pred, for example
  this will return :fred if :fred is in the sequence, otherwise nil:
  (some #{:fred} coll)"
  [pred coll]
  (when-let [s (seq coll)]
    (or (pred (first s))
        (recur pred (next s)))))

(defn comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  ([] identity)
  ([f] f)
  ([f g]
   (fn [& args] (f (apply g args))))
  ([f g & fs]
   (reduce comp (list f g fs))))

(def not-any? (comp not some))
