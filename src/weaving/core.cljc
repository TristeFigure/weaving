(ns weaving.core
  (:require [clojure.set :as set]
            [clojure.walk :refer [postwalk]]
            [clojure.spec.alpha :as s]))

(defn =|
  "Returns a function `(g x)` that returns `(= x v)`."
  [& args]
  #(apply = % args))

(def not|
  "Equivalent to `complement`."
  complement)

(defn *|
  "Equivalent to `juxt`."
  [f & more-fns]
  (let [fns (cons f more-fns)]
    #(apply (apply juxt fns) %&)))

(defn juxtm|
  "Like juxt except that functions are passed as values of a map whose shape
  is used to return their results, under the same key.
  The map can be an explicit map (one argument) or a flat, implicit map
  (variadic arguments)."
  ([m]
   (fn [& args]
     (->> m
          (map (fn [[k v]]
                 [k (apply v args)]))
          (into (empty m)))))
  ([k v & {:as more-kvs}]
   (juxtm| (merge {k v} more-kvs))))

(defn |
  "Returns a function like the result of `partial` except that new args
  are added to the beginning of the parameter list rather than the end."
  [f & args]
  #((apply partial f (concat %& args))))

(defn ||
  "Returns a function that behaves like `partial`."
  [f & args]
  #((apply partial f (concat args %&))))

(defn ø|
  "Unpartialize the first argument of `f`, i.e. returns a function
  that will apply all but the first argument to `f`."
  [f]
  #(apply f (rest %&)))

(defn ø||
  "Unpartialize the last argument of `f`, i.e. returns a function
  that will apply all but the last argument to `f`."
  [f]
  #(apply f (butlast %&)))

;; TODO: rework
(defn args| [f & fs]
  "Returns a function that forwards its args to each `fs` and then passes
  the results to `f` in order."
  (fn [& args]
    (apply f (map (| apply args) fs))))

(defn each-arg| [f arg-f]
  "Returns a function that forwards each of its args to `arg-f` and then
  passes the results to `f` in order."
  (fn [& args]
    (apply f (map arg-f args))))

(defn ->|
  "Returns a function that behaves like `comp` but composes functions
  from left to right."
  [& fns]
  (apply comp (reverse fns)))

(defn <-|
  "Equivalent to `constantly`."
  [v]
  (constantly v))

(defn when|
  "Returns a function that will run the `fns` in order when `pred`
  succeeds or return the value that was passed in otherwise."
  [pred & fns]
  (let [chained-fns (apply ->| fns)]
    #(if (apply pred %&)
       (apply chained-fns %&)
       (if (= (count %&) 1)
         (first %&)
         %&))))

(defn if|
  "Returns a function that will run `f` when `pred` succeeds or
  otherwise run `else` if it is provided (returns the passed value
  by default)."
  ([pred f]
   (if| pred f #(if (= 1 (count %&))
                  (first %&)
                  %&)))
  ([pred f else]
   #(if (apply pred %&)
      (apply f %&)
      (apply else %&))))

(defn apply|
  "Transforms a function `f` accepting one argument, presumably a
  sequence, into a function that applies this argument to `f`."
  [f]
  #(apply f %))

(defn unapply|
  "Transforms a function `f` accepting any number of arguments into a
  function working on these arguments as a list."
  [f]
  #(f %&))

(defn tap|
  "Returns a function that calls `fns` in order, passing to each one
  its argument before returning it."
  ([] identity)
  ([& fns]
   #(do (last ((apply *| (map apply| fns)) %&))
        (if (> (count %&) 1)
          %&
          (first %&)))))

(defn and|
  "Returns a function `f` that runs `fns` in order on the arguments of
  `f` in the style of `and`, i.e. breaking out of the chain upon
  `false` or `nil`."
  ([]  identity)
  ([f] f)
  ([f & more-fns]
   (let [fns (cons f more-fns)]
     #(loop [[f & more] fns]
        (let [result (apply f %&)]
          (if result
            (if (seq more)
              (recur more)
              result)
            result))))))

(defn or|
  "Returns a function that runs `fns` in order in the style of `or`,
  i.e. breaking out of the chain if one returns something different
  than `false` or `nil`."
  ([]  identity)
  ([f] f)
  ([f & more-fns]
   (let [fns (cons f more-fns)]
     (fn [& args]
       (loop [[f & more] fns]
         (let [result (apply f args)]
           (if result
             result
             (if (seq more)
               (recur more)
               result))))))))

(defn call
  "Calls function `f` by applying `args`."
  [f & args]
  (apply f args))


;; TODO: taken from shuriken
(defn- get-nth
  "Like get but also works on lists."
  ([coll k] (get-nth coll k nil))
  ([coll k not-found]
   (if (or (associative? coll) (nil? coll))
     (get coll k not-found)
     (nth coll k not-found))))

(defn- assoc-nth
  "Like assoc but also works on lists.
  Optionally accepts an initial `not-found` argument (defaults to `nil`)."
  ([coll n v] (assoc-nth {} coll n v))
  ([not-found coll n v]
   (let [coll (if (nil? coll) not-found coll)]
     (if (associative? coll)
       (assoc coll n v)
       (do (when-not (<= n (count coll))
             (throw (new #?@(:clj [IndexOutOfBoundsException]
                             :cljs [js/Error "out of bounds"]))))
           (concat (take n coll)
                   [v]
                   (drop (inc n) coll)))))))

(defn- update-nth-in
  "Like update-in but also works on lists.
  Optionally accepts an initial `not-found-f` argument, a fn of signature
    `(fn [path coll])`
  (defaults to `(constantly {})`)."
  [& args]
  (let [{:keys [not-found-f m ks f args] :or {not-found-f (constantly {})}}
        (s/conform (s/cat
                     :not-found-f (s/? ifn?)
                     :m           (or| coll? nil?)
                     :ks          sequential?
                     :f           ifn?
                     :args        (s/* any?))
                   args)
        up (fn up [pth m ks f args]
             (let [[k & ks] ks
                   pth (conj pth k)]
               (if ks
                 (assoc-nth (not-found-f pth m) m k
                            (up pth (get-nth m k) ks f args))
                 (assoc-nth (not-found-f pth m) m k
                            (apply f (get-nth m k) args)))))]
    (up [] m ks f args)))

(defn in|
  "Transforms a one-argument function `f` into a function receiving
  one collection and applying `f` to the value at the location
  denoted by `path` in this collection.
  Multiple path+function pairs can follow. "
  [path f & more]
  (let [g #(update-nth-in % path f)]
    (if (seq more)
      (->| g (apply in| more))
      g)))


;; TODO: nested calls to (%| ...) and #(...)
;; IMP:  some computations are repeated unnecessarily
;; IMP:  use a dance
(defn parse-int [x]
  #?(:clj  (Integer/parseInt x)
     :cljs (js/parseInt x)))

(defmacro %| [& expr]
  (let [%-syms      (atom {})
        args-by-num (fn [m]
                      (->> (keep  #(when (-> % key number?) %)  m)
                           sort))
        next-num    (fn [m]
                      (-> m args-by-num last
                          (as-> e (if e (key e) 0))
                          inc))
        new-expr
        (clojure.walk/postwalk
          (fn [form]
            (if-let [[_ n nme] (and (simple-symbol? form)
                                    (when-let [v (re-matches #"%(\d+)?(\D.*)?"
                                                             (name form))]
                                      (update
                                        v 1 #(when % (parse-int %)))))]
              (letfn [(handle-n [m]
                                (if (and n (not (m n)))
                                  (assoc m n (symbol (str \% n nme)))
                                  m))
                      (handle-nme [m]
                                  (if (and nme (not (m nme)))
                                    (let [n (or n (next-num m))]
                                      (as-> m m
                                        (assoc m nme
                                          (or (m n) (symbol (str \% n nme))))
                                        (assoc m n (m nme))))
                                    m))
                      (handle-none [m]
                                   (if (and (not n) (not nme))
                                     (assoc m 1 (symbol "%1"))
                                     m))]
                (get (swap! %-syms (->| handle-n handle-nme handle-none))
                     (or n nme 1)))
              form))
          expr)
        %-syms        (deref %-syms)
        used-args     (vals %-syms)
        arg-count     (->> used-args sort last name
                           (re-matches #"%(\d+).*")
                           second parse-int)
        args          (vec (for [n (range 1 (inc arg-count))]
                             (get %-syms n (symbol (str \% n)))))]
    `(fn ~args
       ~new-expr)))

;; replaced by in|
; (defn bounce| [pos f]
;   (fn [& args]
;     (let [pos (if (sequential? pos) pos (vector pos))]
;       (assoc-in (vec args) pos (apply f args)))))

; (defn transpose [coll-of-colls]
;   (apply map #(apply list %&) coll-of-colls))

; (defn coshaped? [form & more-forms]
;   (every? (condp call form
;             sequential?  sequential?
;             associative? associative?
;             coll?        coll?
;             (constantly true))
;           more-forms))

; (defn map-aligned [f m & ms]
;   (map (fn [e]
;          (let [k (key e)
;                other-es (map #(find % k) ms)]
;            (apply f e other-es)))
;        m))

; (defn map+ [f & [form :as forms]]
;   (letfn [(safe| [f]
;                  (fn [& in-items]
;                    (let [out-items (apply f in-items)]
;                      (assert (= (count in-items) (count out-items)))
;                      (assert (apply coshaped? (cons (first in-items)
;                                                     out-items)))
;                      out-items)))]
;     (condp call form
;       sequential?  (let [min-length (apply min (map count forms))]
;                      (map #(concat %1 (drop min-length %2))
;                           (transpose (apply map (safe| f) forms))
;                           forms))
;       associative? (let [ks        (map (comp set keys) forms)
;                          all-ks    (apply set/union ks)
;                          common-ks (apply set/intersection ks)
;                          common-ms (map #(select-keys % common-ks) forms)
;                          unique-ks (set/difference all-ks common-ks)
;                          unique-ms (map #(select-keys % unique-ks) forms)]
;                      (->> (apply map-aligned (safe| f) common-ms)
;                           transpose
;                           (map #(concat %2 %1) unique-ms))))))

; ;; TODO: handle records
; (defn comap [f & colls]
;   (assert (apply coshaped? colls))
;   (letfn [(reshape [original anew]
;                    (condp call original
;                      list?      (apply list anew)
;                      map-entry? (vec anew)
;                      seq?       (doall anew)
;                      coll?      (into (empty original) anew)))]
;     (->> (apply map+ f colls)
;          (map reshape colls))))

; (defn cowalk [inner outer & [form :as forms]]
;   {:test
;    (fn []
;      (is (= [[2 2 2] [1 1 1]] (cowalk (bounce| 0 +) #(do %&) [1 1 1] [1 1 1])))
;      (is (= [[2 2 1] [1 1]]   (cowalk (bounce| 0 +) #(do %&) [1 1 1] [1 1])))
;      (is (= [[1 1]   [2 2 1]] (cowalk (bounce| 1 +) #(do %&) [1 1] [1 1 1])))
;      (is (= [{:a 2 :b 4} {:a 1 :b 2 :c 3}]
;             (cowalk (bounce| 0 (fn [& [[k _v] :as es]]
;                                  [k (apply + (map val es))]))
;                     #(do %&)
;                     {:a 1 :b 2}
;                     {:a 1 :b 2 :c 3}))))}
;   (if (coll? form)
;     (apply outer (apply comap inner forms))
;     (apply outer forms)))

; (defn copostwalk [f & forms]
;   {:test (fn []
;            (is (= [[1 2 4] [identity identity inc]]
;                   (copostwalk (bounce| 0 #(if  (number? %1)  (%2 %1)  %1))
;                               [1 2 3]
;                               [identity identity inc]))))}
;   (apply cowalk (partial copostwalk f) f forms))

; (defn coprewalk [f & forms]
;   {:test (fn []
;            (is (= [[1 2 4] [identity identity inc]]
;                   (coprewalk (bounce| 0 #(if  (number? %1)  (%2 %1)  %1))
;                              [1 2 3]
;                              [identity identity inc]))))}
;   (apply cowalk (partial coprewalk f) #(do %&) (apply f forms)))

; (defn deep|* [funcs-tree]
;   (fn [& data-tree]
;     (first (copostwalk (bounce| 0 #(if  (or (fn? %2) (map? %2))  (%2 %1)  %1))
;                        data-tree
;                        funcs-tree))))

; (defn args-identity [& args]
;   (if (= (count args) 1)
;     (first args)
;     args))

; (defmacro deep| [funcs]
;   {:test (fn []
;            (is (= [1 2 [4]]   ((deep| [_ _ [inc]])  1 2 [3]))))}
;   (let [funcs (if (coll? funcs)
;                 (clojure.walk/postwalk
;                   #(if  (and (instance? clojure.lang.Named %)
;                              (re-matches #"_.*" (name %)))
;                      `args-identity
;                      %)
;                   funcs)
;                 funcs)]
;     `(deep|* ~funcs)))

; (deftest test-deep|
;   (is (= [1 2 [4]]   ((deep| [_truc _machin [inc]])  1 2 [3])))
;   ((fn []
;        (is (= [[2 2 2] [1 1 1]] (cowalk (bounce| 0 +) #(do %&) [1 1 1] [1 1 1])))
;        (is (= [[2 2 1] [1 1]]   (cowalk (bounce| 0 +) #(do %&) [1 1 1] [1 1])))
;        (is (= [[1 1]   [2 2 1]] (cowalk (bounce| 1 +) #(do %&) [1 1] [1 1 1])))
;        (is (= [{:a 2 :b 4} {:a 1 :b 2 :c 3}]
;               (cowalk (bounce| 0 (fn [& [[k _v] :as es]]
;                                    [k (apply + (map val es))]))
;                       #(do %&)
;                       {:a 1 :b 2}
;                       {:a 1 :b 2 :c 3}))))))
