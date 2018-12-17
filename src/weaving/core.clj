(ns weaving.core
  (:require [arity.core :refer [arities fake-arities max-arity min-arity]]))

(defn ?|
  "Returns a function `(g x)` that returns `(= x v)`."
  [& args]
  #(apply = % args))

(defn not|
  "Returns a function that behaves like `complement` but preserves
  arity."
  [f]
  (fake-arities (arities f) (complement f)))

(defn *|
  "Returns a function that behaves like `juxt` but preserves arity."
  [f & more-fns]
  (let [fns (cons f more-fns)]
    (fake-arities (->> fns (mapcat arities) distinct sort)
                  #(apply (apply juxt fns) %&))))

(defn ||
  "Returns a function that behaves like `partial` but preserves arity."
  [f & args]
  (fake-arities (map #(- % (count args))
                   (arities f))
              #((apply partial f (concat args %&)))))

(defn |
  "Returns a function that behaves like `partial` except that new args
  are added to the beginning of the parameter list rather than the end.
  Preserves arity."
  [f & args]
  (fake-arities (map #(- % (count args))
                   (arities f))
              #((apply partial f (concat %& args)))))

;; TODO: rework
(defn •|
  "Transforms a function whose arguments become calls to functions that
  will be passed the woven value."
  [f & fs]
  (fn [& args]
    (apply f (map #(apply % args) fs))))

(defn arity-comp
  "Composes functions like `comp` but preserves arity."
  ([]  identity)
  ([f] f)
  ([f & more-fns]
   (let [fns (cons f more-fns)]
     (fake-arities (-> fns last arities)
                   #(apply (apply comp fns)
                           %&)))))

(defn ->|
  "Returns a function that behaves like `comp` but composes functions
  from left to right.
  Preserves arity."
  [& fns]
  (apply arity-comp (reverse fns)))

(defn <-|
  "Returns a function that behaves like `constantly` but has 1 for
  arity."
  [v]
  (fake-arities 1 (constantly v)))

(defn when|
  "Returns a function that will run the `fns` in order when `pred`
  succeeds or return the value that was passed in otherwise.
  Preserves arity."
  [pred & fns]
  (fake-arities
    (->> (cons pred fns) (mapcat arities) distinct sort)
    #(if (apply pred %&)
       (apply (apply ->| fns) %&)
       (if (= (count %&) 1)
         (first %&)
         %&))))

(defn if|
  "Returns a function that will run `f` when `pred` succeeds or
  otherwise run `else` if it is provided (defaults to `nil`).
  Preserves arity."
  ([pred f]
   (if| pred f #(if (= 1 (count %&))
                  (first %&)
                  %&)))
  ([pred f else]
   (fake-arities
     (->> [pred f else] (mapcat arities) distinct sort)
     #(if (apply pred %&)
        (apply f %&)
        (apply else %&)))))

(defn apply|
  "Transforms a function `f` accepting one argument, presumably a
  sequence, into a function that applies this argument to `f`."
  [f]
  #(apply f %1))

(defn tap|
  "Returns a function that calls `fns` in order,
  passing to each one its argument before returning it.
  Preserves arity."
  ([] identity)
  ([& fns]
   (fake-arities
     (->> fns (mapcat arities) distinct sort)
     #(do (last ((apply *| (map apply| fns)) %&))
          (if (> (count %&) 1)
            %&
            (first %&))))))

(defn and|
  "Returns a function `f` that runs `fns` in order on the arguments of
  `f` in the style of `and`, i.e. breaking out of the chain upon
  `false` or `nil`.
  Preserves arity."
  ([]  identity)
  ([f] f)
  ([f & more-fns]
   (let [fns (cons f more-fns)]
     (fake-arities
       (->> fns (mapcat arities) distinct sort)
       #(loop [[f & more] fns]
          (let [result (apply f %&)]
            (if result
              (if (seq more)
                (recur more)
                result)
              result)))))))

(defn or|
  "Returns a function that runs `fns` in order in the style of `or`,
  i.e. breaking out of the chain if one returns something different
  than `false` or `nil`.
  Preserves arity."
  ([]  identity)
  ([f] f)
  ([f & more-fns]
   (let [fns (cons f more-fns)]
     (fake-arities
       (->> fns (mapcat arities) distinct sort)
       (fn [& args]
         (loop [[f & more] fns]
           (let [result (apply f args)]
             (if result
               result
               (if (seq more)
                 (recur more)
                 result)))))))))

(defn- wrap-context [[form ctx]]
  [::context form ctx])

(defn- unwrap-context [x]
  (-> x rest vec))

(defn- wrapped-context? [x]
  (and (vector? x)
       (-> x first (= ::context))))

(defn- context-wrapper [f]
  (let [wrap-f (fn wrap
                 ([form]     (wrap form nil))
                 ([form ctx] (let [result (f form ctx)]
                               (if (wrapped-context? result)
                                 (unwrap-context result)
                                 result))))]
    (fake-arities (arities f)
      #(apply wrap-f %&))))

;; TODO: document
(defn context| [f]
  (let [ar (set (arities f))
        mono-ar (or (contains? ar 1)
                    (contains? ar ##Inf))
        bi-ar (and (contains? ar 2)
                   (not (contains? ar ##Inf)))
        new-f (case [mono-ar bi-ar]
                [true true]   f
                [true false]  (fn
                                ([x]     (wrap-context [(f x) nil]))
                                ([x ctx] (wrap-context [(f x) ctx])))
                [false true]  (fn
                                ([x]     (wrap-context [(f x) nil]))
                                ([x ctx] (f x ctx)))
                [false false] (fn
                                ([x]     (wrap-context [(f x) nil]))
                                ([x ctx] (wrap-context [(f x) ctx]))))]
    (fake-arities (arities new-f)
                (fn [& args]
                  (let [r (apply (context-wrapper new-f)
                                 args)]
                    r)))))

;; TODO: test and document
(defn warp| [weaver warper]
  (fn [& fns]
    (apply weaver (map (fn [f]
                         (fake-arities (arities f)
                                     (fn [& args]
                                       (apply warper f args))))
                       fns))))
;      ;   (if (even? x)
;      ;     (f x)
;      ;     x))
;      ; inc
;      )
;    x))
