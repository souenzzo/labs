(ns br.com.souenzzo.cljc-core
  (:refer-clojure :exclude [* *' *1 *2 *3 *agent* *allow-unresolved-vars* *assert* *clojure-version*
                            *compile-files* *compile-path* *compiler-options* *data-readers* *default-data-reader-fn*
                            *err* *file* *flush-on-newline* *fn-loader* *in* *math-context* *ns* *out* *print-dup*
                            *print-level* *print-meta* *print-namespace-maps* *print-readably* *read-eval*
                            *source-path* *suppress-read* *unchecked-math* *use-context-classloader*
                            *warn-on-reflection* + +' - -' -> ->> ->ArrayChunk ->Eduction ->Vec ->VecNode ->VecSeq
                            -reset-methods .. / < <= == > >= EMPTY-NODE Inst PrintWriter-on StackTraceElement->vec
                            accessor aclone add-classpath add-tap add-watch agent agent-error agent-errors aget
                            alias all-ns alter alter-meta! alter-var-root amap ancestors and any? areduce
                            as-> aset aset-boolean aset-byte aset-char aset-double aset-float aset-int aset-long
                            assert assoc assoc-in associative? atom await await-for await1 bases bean bigdec
                            biginteger binding bit-and bit-and-not bit-clear bit-flip bit-not bit-or bit-set
                            bit-shift-right bit-test bit-xor boolean boolean-array boolean? booleans bound-fn
                            bound? bounded-count butlast byte byte-array bytes bytes? case cast cat char char-array
                            char-name-string char? chars chunk chunk-append chunk-buffer chunk-cons chunk-first
                            chunk-rest chunked-seq? class class? clear-agent-errors clojure-version coll? comment
                            comp comparator compare compare-and-set! compile complement completing concat cond cond->
                            condp conj! constantly construct-proxy contains? count counted? create-ns
                            cycle dec dec' decimal? declare dedupe default-data-readers definline definterface
                            defmethod defmulti defonce defprotocol defrecord defstruct deftype delay
                            deliver denominator deref derive descendants destructure disj disj! dissoc dissoc!
                            distinct? doall dorun doseq dosync dotimes doto double double-array double? doubles drop
                            drop-while eduction empty empty? ensure ensure-reduced enumeration-seq error-handler
                            eval even? every-pred every? ex-cause ex-data ex-message extend extend-protocol
                            extenders extends? false? ffirst file-seq filter filterv find find-keyword find-ns
                            find-protocol-method find-var flatten float float-array float? floats flush fn?
                            fnil for force format frequencies future future-call future-cancel future-cancelled?
                            future? gen-class gen-interface gensym get-in get-method get-proxy-class
                            get-validator group-by halt-when hash hash-combine hash-map hash-ordered-coll hash-set
                            ident? identical? identity if-let if-not if-some ifn? import in-ns inc inc' indexed?
                            inst-ms inst-ms* inst? instance? int int-array int? integer? interleave intern interpose
                            into-array ints io! isa? iterate iterator-seq juxt keep keep-indexed key keys keyword
                            last lazy-cat lazy-seq letfn line-seq list* list? load load-file load-reader
                            loaded-libs locking long long-array longs loop macroexpand macroexpand-1 make-array
                            map map-entry? map-indexed map? mapcat mapv max max-key memfn memoize merge merge-with
                            method-sig methods min min-key mix-collection-hash mod munge name namespace
                            nat-int? neg-int? neg? newline nfirst nnext not not-any? not-empty not-every?
                            ns ns-aliases ns-imports ns-interns ns-map ns-name ns-publics ns-refers ns-resolve
                            ns-unmap nth nthnext nthrest num number? numerator object-array odd? or parents partial
                            partition-all partition-by pcalls peek pmap pop pop! pop-thread-bindings
                            pos? pr pr-str prefer-method prefers primitives-classnames print print-ctor print-dup
                            print-simple print-str printf println println-str prn prn-str promise proxy
                            proxy-mappings proxy-name proxy-super push-thread-bindings pvalues qualified-ident?
                            qualified-symbol? quot rand rand-int rand-nth random-sample range ratio? rational?
                            re-find re-groups re-matcher re-matches re-pattern re-seq read read+string read-line
                            reader-conditional reader-conditional? realized? record? reduce-kv
                            reductions ref ref-history-count ref-max-history ref-min-history ref-set refer
                            reify release-pending-sends rem remove remove-all-methods remove-method remove-ns
                            remove-watch repeat repeatedly replace replicate require requiring-resolve reset!
                            reset-vals! resolve rest restart-agent resultset-seq reverse reversible? rseq rsubseq
                            satisfies? second select-keys send send-off send-via seq? seqable? seque
                            set set-agent-send-executor! set-agent-send-off-executor! set-error-handler!
                            set-validator! set? short short-array shorts shuffle shutdown-agents simple-ident?
                            simple-symbol? slurp some some-> some->> some-fn some? sort sort-by sorted-map
                            sorted-set sorted-set-by sorted? special-symbol? spit split-at split-with str string?
                            struct-map subs subseq subvec supers swap! swap-vals! symbol symbol? sync tagged-literal
                            take take-last take-nth take-while tap> test the-ns thread-bound? time to-array
                            trampoline transduce tree-seq true? type unchecked-add unchecked-add-int
                            unchecked-char unchecked-dec unchecked-dec-int unchecked-divide-int unchecked-double
                            unchecked-inc unchecked-inc-int unchecked-int unchecked-long unchecked-multiply
                            unchecked-negate unchecked-negate-int unchecked-remainder-int unchecked-short
                            unchecked-subtract-int underive unquote unquote-splicing unreduced
                            update update-in update-proxy uri? use uuid? val vals var-get var-set var? vary-meta vec
                            vector-of vector? volatile? vswap! when when-first when-let when-not
                            while with-bindings with-bindings* with-in-str with-loading-context with-local-vars
                            with-open with-out-str with-precision with-redefs with-redefs-fn xml-seq zero? zipmap])
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


(defn- preserving-reduced
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
