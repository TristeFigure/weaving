(ns weaving.core-test
  (:require [clojure.test :refer :all]
            [weaving.core :refer :all]
            [arity.core :refer [arities]]
            [shuriken.test :refer :all]))


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
  (is (= 1   ((<-| 1) :ignored :ditto)))
  (is (= [1] (arities (<-| 1)))))

(deftest test-?|
  (is (= true  ((?| 1) 1)))
  (is (= false ((?| 2) 1))))

(deftest test-not|
  (is (false? ((not| number?) 3)))
  (testing "preserves arity"
    (is (= [1]     (arities (not| (fn [a])))))
    (is (= [5]     (arities (not| (fn [a b c d e])))))
    (is (= [##Inf] (arities (not| (fn [& args])))))))

(deftest test-*|
  (is   (= [4 2 -3]  ((*| inc dec -) 3)))
  (testing "preserves arity"
    (is (= [2 ##Inf] (arities (*| (fn [a b]) (fn [& args])))))))

(deftest test-|
  (is   (= "110100"   ((| str 1 10 100))))
  (is   (= "100110"   ((| str 1 10) 100)))
  (is   (= "101001"   ((| str 1) 10 100)))
  (is   (= "110100"   ((| str) 1 10 100)))
  (testing "preserves arity"
    (is (= [2]   (arities (| (fn [a b c]) 1))))
    (is (= [1]   (arities (| (fn [a b c]) 1 2))))
    (is (= [0]   (arities (| (fn [a b c]) 1 2 3))))))

(deftest test-||
  (is   (= "110100"   ((|| str 1 10 100))))
  (is   (= "110100"   ((|| str 1 10) 100)))
  (is   (= "110100"   ((|| str 1) 10 100)))
  (is   (= "110100"   ((|| str) 1 10 100)))
  (testing "preserves arity"
    (is (= [2]   (arities (|| (fn [a b c]) 1))))
    (is (= [1]   (arities (|| (fn [a b c]) 1 2))))
    (is (= [0]   (arities (|| (fn [a b c]) 1 2 3))))))

(deftest test-•|
  (is (= {:a 1 :b 2}
         ((•| merge identity (<-| {:b 2}))
          {:a 1}))))

(deftest test-arity-comp
  (= 4 ((arity-comp inc (fn [a b] (+ a b)))
        1 2))
  (testing "preserves arity"
    (is (= [2]     (arities (arity-comp inc (fn [a b] (+ a b))))))
    (is (= [##Inf] (arities (arity-comp inc (fn [& more] 0)))))))

(deftest test-->|
  (is (= ((->| - inc inc) 4)
         ((comp inc inc -) 4)))
  (is (= ((->| inc inc -) 4)
         ((comp - inc inc) 4)))
  (testing "preserves arity"
    (is (= [2]     (arities (->| (fn [a b] (+ a b)) inc))))
    (is (= [##Inf] (arities (->| (fn [& args] 0) inc))))))

(deftest test-apply|
  (is (= [2 1] ((->| (fn xx [x] [x x])
                     (apply| (fn xxx [a b] [(inc a) b])))
                1)))
  (testing "has arity 1"
    (is (= [1] (arities (->| (fn xx [x] [x x])
                         (apply| (fn xxx [a b] [(inc a) b]))))))))

(deftest test-when|
  (is (= 11 ((when| number? identity inc) 10)))
  (is (= :a ((when| number? identity inc) :a)))
  (testing "preserves arity"
    (is (= [2 ##Inf] (arities (when| (fn [a b] true)    (constantly :abc)))))
    (is (= [##Inf]   (arities (when| (fn [& more] true) (constantly :abc)))))))

(deftest test-if|
  (is (= 11 ((if| number? inc str) 10)))
  (is (= ":a" ((if| number? inc str) :a)))
  (is (= :a ((if| number? inc) :a)))
  (testing "preserves arity"
    (is (= [2 ##Inf] (arities (if| (fn [a b] true)    (constantly :abc)))))
    (is (= [##Inf]   (arities (if| (fn [& more] true) (constantly :abc)))))))

(deftest test-tap|
  (with-fresh-calls
    (is (= 1 ((tap| inc do-nothing do-nothing) 1)))
    (assert-calls [:do-nothing :do-nothing]))
  (testing "preserves arity"
    (is (= [2 ##Inf] (arities (tap| (fn [a b])     (constantly 0)))))
    (is (= [##Inf]   (arities (tap| (constantly 0) (fn [& more] more)))))))

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
  (testing "preserves arity"
    (is (= [2 ##Inf] (arities (and| (fn [a b] true)    (constantly true)))))
    (is (= [##Inf]   (arities (and| (fn [& more] true) (constantly true)))))))

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
  (testing "preserves arity"
    (is (= [2 ##Inf] (arities (or| (fn [a b] true)    (constantly true)))))
    (is (= [##Inf]   (arities (or| (fn [& more] true) (constantly true)))))))

(deftest test-context|
  (is (= [124 {}]  ((context| inc)                         123 {})))
  (is (= [124 {}]  ((context| (context| inc))              123 {})))
  (is (= [124 nil] ((context| inc)                         123)))
  (is (= [124 nil] ((context| (context| inc))              123)))
  (is (= [0   {}]  ((context| (fn [& args] 0))             123 {})))
  (is (= [0   nil] ((context| (fn [& args] 0))             123)))
  (is (= [0   {}]  ((context| (fn [a b] [0 b]))            123 {})))
  (is (= [0   nil] ((context| (fn [a b] [0 b]))            123)))
  (is (= [0   nil] ((context| (context| (fn [a b] [0 b]))) 123)))
  (is (= [103 {}]  ((->| (context| inc)
                         (apply| (context| (fn [a ctx] [(inc a) ctx])))
                         (apply| (context| (fn [a ctx] [(inc a) ctx]))))
                    100 {})))
  (testing "has 1 and 2 for arities"
    (is (= [1 2] (arities (->| (context| inc))))))
  (testing "with 1, 2 and ##Inf arities"
    (is (= [1 {}] ((context| (fn ([a] a) ([a b] [a 0]) ([a b & c])))
                   1 {})))))
