(ns weaving.core-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test    :refer-macros [deftest is are testing]])
            [weaving.core :refer [*| juxtm| <-| =| not| | || ø| ø|| ->|
                                  apply| unapply| when| if| tap| and|
                                  or| call in| %| args|]])
  #?(:cljs
      (:require-macros
        [weaving.core-test :refer [defn-call with-fresh-calls]])))

(def calls
  (atom []))

(defn store-call! [v]
  (swap! calls conj v))

(defn assert-calls [vs]
  (is (= vs @calls)))

#?(:clj (defmacro with-fresh-calls [& body]
          `(do (reset! calls [])
               ~@body)))

#?(:clj (defmacro defn-call [name params & body]
          `(defn ~name ~params
             (let [result# (do ~@body)]
               (store-call! ~(keyword name))
               result#))))

#?(:clj (defmacro with-ns
          "Evaluates body in another namespace. ns is either a namespace
          object or a symbol.  Useful to define functions in namespaces other
          than `*ns*`."
          [ns & body]
          `(binding [*ns* (find-ns ~ns)]
             (eval (quote (do ~@body))))))

(def john
  {:name "john"
   :age 32
   :gender "male"})

(defn-call is-john? [person]
  (= "john" (:name person)))

(defn-call is-not-john? [person]
  (not= "john" (:name person)))

(defn-call is-adult? [person]
  (>= (:age person) 18))

(defn-call is-minor? [person]
  (< (:age person) 18))

(defn-call is-male? [person]
  (= "male" (:gender person)))

(defn-call is-female? [person]
  (= "female" (:gender person)))

(defn-call do-nothing [x]
  x)


(deftest test-<-|
  (is (= 1   ((<-| 1)))))

(deftest test-=|
  (is (= true  ((=| 1) 1)))
  (is (= false ((=| 2) 1)))
  (testing "multiple equality"
    (is (= true ((=| 2 (inc 1)) (+ 1 1)))))
  (testing "edge cases"
    (is (= true ((=|) 1)))))

(deftest test-not|
  (is (false? ((not| number?) 3))))

(deftest test-*|
  (is (= [4 2 -3]  ((*| inc dec -) 3))))

(deftest test-juxtm|
  (testing "with an explicit map"
    (is (= {:a 2 :b 0} ((juxtm| {:a inc :b dec}) 1))))
  (testing "with a flat, impicit map"
    (is (= {:a 2 :b 0} ((juxtm|  :a inc :b dec ) 1)))))

(deftest test-|
  (is   (= "110100"   ((| str 1 10 100))))
  (is   (= "100110"   ((| str 1 10) 100)))
  (is   (= "101001"   ((| str 1) 10 100)))
  (is   (= "110100"   ((| str) 1 10 100))))

(deftest test-||
  (is   (= "110100"   ((|| str 1 10 100))))
  (is   (= "110100"   ((|| str 1 10) 100)))
  (is   (= "110100"   ((|| str 1) 10 100)))
  (is   (= "110100"   ((|| str) 1 10 100))))

(deftest test-ø|
  (is (= "234" ((ø| str) 1 2 3 4)))
  (is (= ""    ((ø| str) 1)))
  (is (= ""    ((ø| str))))
  (is (= "3"   ((-> str ø| ø|) 1 2 3)))
  (is (= ""    ((-> str ø| ø| ø|) 1 2 3))))

(deftest test-ø||
  (is (= "123" ((ø|| str) 1 2 3 4)))
  (is (= ""    ((ø|| str) 4)))
  (is (= ""    ((ø|| str))))
  (is (= "1"   ((-> str ø|| ø||) 1 2 3)))
  (is (= ""    ((-> str ø|| ø|| ø||) 1 2 3))))

(deftest test-args|
  (is (= {:a 1 :b 2}
         ((args| merge identity (<-| {:b 2}))
          {:a 1}))))

(deftest test-->|
  (is (= ((->| - inc inc) 4)
         ((comp inc inc -) 4)))
  (is (= ((->| inc inc -) 4)
         ((comp - inc inc) 4)))
  (testing "edge cases"
    (is (= identity (->|)))))

(deftest test-apply|
  (is (= [2 1] ((->| (fn xx [x] [x x])
                     (apply| (fn xxx [a b] [(inc a) b])))
                1))))

(deftest test-unapply|
  (is (= #{1 2 3} ((unapply| set) 1 2 3))))

(deftest test-when|
  (is (= 11 ((when| number? identity inc) 10)))
  (is (= :a ((when| number? identity inc) :a))))

(deftest test-if|
  (is (= 11   ((if| number? inc str) 10)))
  (is (= ":a" ((if| number? inc str) :a)))
  (is (= :a   ((if| number? inc) :a))))

(deftest test-tap|
  (with-fresh-calls
    (is (= 1 ((tap| inc do-nothing do-nothing) 1)))
    (assert-calls [:do-nothing :do-nothing]))
  (testing "edge cases"
    (is (= identity (tap|)))))

(deftest test-and|
  (testing "when each predicate is true"
    (with-fresh-calls
      (let [composite (and| is-john? is-adult? is-male?)]
        (is (= true (composite john)))
        ;; calls  all predicates
        (assert-calls [:is-john? :is-adult? :is-male?]))))
  (testing "when one of the predicates fail"
    (with-fresh-calls
      (let [composite (and| is-john? is-minor? is-male?)]
        (is (= false (composite john)))
        ;; stops at the predicate that fails
        (assert-calls [:is-john? :is-minor?]))))
  (testing "edge cases"
    (is (= identity (and|)))))

(deftest test-or|
  (testing "when each predicate is false"
    (with-fresh-calls
      (let [composite (or| is-not-john? is-minor? is-female?)]
        (is (= false (composite john)))
        ;; calls  all predicates
        (assert-calls [:is-not-john? :is-minor? :is-female?]))))
  (testing "when one of the predicates succeds"
    (with-fresh-calls
      (let [composite (or| is-not-john? is-adult? is-female?)]
        (is (= true (composite john)))
        ;; stops at the predicate that fails
        (assert-calls [:is-not-john? :is-adult?]))))
  (testing "edge cases"
    (is (= identity (or|)))))

(deftest test-call
  (is (= 2 (call inc 1))))

(deftest test-in|
  (is (= [0 [0 [1]]]
         ((in| [1 1 0] inc) [0 [0 [0]]])))
  (is (= [0 [0 {:a [0 1] :b 1}]]
         ((in| [1 1 :a 1] inc
               [1 1 :b]   inc)
          [0 [0 {:a [0 0] :b 0}]]))))

(deftest test-%|
  (are [x y] (= y #?(:clj  (with-ns 'weaving.core (macroexpand x))
                     :cljs (macroexpand x)))
       '(%| + %1 %2)         '(fn* ([%1 %2]     (+ %1 %2)))
       '(%| + %  %2)         '(fn* ([%1 %2]     (+ %1 %2)))
       '(%| + %a %b)         '(fn* ([%1a %2b]   (+ %1a %2b)))
       ;; unused args
       '(%| + %2b %3c)       '(fn* ([%1 %2b %3c] (+ %2b %3c)))
       ;; a %NUM can refer to a previous %NAME
       '(%| + %abc %2 %1)    '(fn* ([%1abc %2] (+ %1abc %2 %1abc)))
       ;; a %NAME never refer to a previous %NUM
       '(%| + %1 %2 %abc)    '(fn* ([%1 %2 %3abc] (+ %1 %2 %3abc)))
       ;; but a %NAME can refer to a previous %NUMNAME
       '(%| + %1abc %2 %abc) '(fn* ([%1abc %2] (+ %1abc %2 %1abc)))
       ;; and a %NUMNAME can refer to a previous %NUM
       '(%| + %1 %2 %1abc)   '(fn* ([%1 %2] (+ %1 %2 %1)))))

; ;; TODO: finish or remove
; ; (deftest test-context|
; ;   (is (= [124 {}]  ((context| inc)                         123 {})))
; ;   (is (= [124 {}]  ((context| (context| inc))              123 {})))
; ;   (is (= [124 nil] ((context| inc)                         123)))
; ;   (is (= [124 nil] ((context| (context| inc))              123)))
; ;   (is (= [0   {}]  ((context| (fn [& args] 0))             123 {})))
; ;   (is (= [0   nil] ((context| (fn [& args] 0))             123)))
; ;   (is (= [0   {}]  ((context| (fn [a b] [0 b]))            123 {})))
; ;   (is (= [0   nil] ((context| (fn [a b] [0 b]))            123)))
; ;   (is (= [0   nil] ((context| (context| (fn [a b] [0 b]))) 123)))
; ;   (is (= [103 {}]  ((->| (context| inc)
; ;                          (apply| (context| (fn [a ctx] [(inc a) ctx])))
; ;                          (apply| (context| (fn [a ctx] [(inc a) ctx]))))
; ;                     100 {})))
; ;   (testing "has 1 and 2 for arities"
; ;     (is (= [1 2] (arities (->| (context| inc))))))
; ;   (testing "with 1, 2 and ##Inf arities"
; ;     (is (= [1 {}] ((context| (fn ([a] a) ([a b] [a 0]) ([a b & c])))
; ;                    1 {})))))
