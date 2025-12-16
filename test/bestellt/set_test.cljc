(ns bestellt.set-test
  (:require
   [bestellt.set :as bset]
   [clojure.test :as t]
   [clojure.test.check.generators :as gen]
   #?(:clj [collection-check.core :as ct])
   #?(:cljs [cljs.reader :refer [read-string]])))

#?(:clj
   (t/deftest check
     (ct/assert-set-like (bset/set) gen/int)))

#?(:clj
   (t/deftest implementations
     (let [s (bset/set)]
       (t/testing "Interfaces marked as implemented"
         (t/are [class] (instance? class s)
           clojure.lang.IPersistentSet
           clojure.lang.IPersistentCollection
           clojure.lang.Counted
           java.util.Set))
       (t/testing "Behavior smoke testing"
         (t/testing "Most operations don't change type"
           (t/are [object] (= (class object) (class s))
             (conj s 1 2)
             (disj s 1)
             (into s #{1 2})))
         (t/testing "Seq-oriented operations return nil when empty"
           (t/are [object] (nil? object)
             (seq s)
           (rseq s)))))))

(t/deftest equality
  (let [empty (bset/set)
        one-item (conj empty 1)]
    (t/testing "Basic symmetric equality"
      (t/is (= #{} empty))
      (t/is (= empty #{}))
      (t/is (= #{1} one-item))
      (t/is (= one-item #{1})))
    (t/testing "Order-insensitive comparisons"
      (let [one-way (into empty [1 2 3 4])
            other-way (into empty [3 4 1 2])
            unsorted #{1 2 3 4}]
        (t/is (= one-way other-way))
        (t/is (= one-way unsorted))
        (t/is (= other-way unsorted))))
    (t/testing "Does not blow up when given something random"
      (t/is (not= one-item 'baz))
      (t/is (not= 'baz one-item)))))

(t/deftest ordering
  (let [values [[:first 10]
                [:second 20]
                [:third 30]]
        s (into (bset/set) values)]
    (t/testing "Seq behaves like seq of a vector"
      (t/is (= (seq values) (seq s))))
    (t/testing "New values get added at the end"
      (let [entry [:fourth 40]]
        (t/is (= (seq (conj values entry))
               (seq (conj s entry))))))
    (t/testing "Re-adding keys leaves them in the same place"
      (t/is (= (seq s)
             (seq (conj s [:second 20])))))
    (t/testing "Large number of keys still sorted"
      (let [ints (range 5000)
            ordered (into s ints)]
        (= (seq ints) (seq ordered))))))

(t/deftest reversing
  (let [source (vec (range 1000))
        s (into (bset/set) source)]
    (t/is (= (rseq s) (rseq source)))))

(t/deftest set-features
  (let [s (bset/set :a 1 :b 2 :c 3)]
    (t/testing "Keyword lookup"
      (t/is (= :a (:a s))))
    (t/testing "IFn support"
      (t/is (= :b (s :b))))
    (t/testing "Falsy lookup support"
      (t/is (= false ((bset/set false 1) false))))
    (t/testing "Ordered disj"
      (t/is (= #{:a 1 2 3} (disj s :b :c))))
    (t/testing "meta support"
      (t/is (= {'a 'b} (meta (with-meta s {'a 'b})))))
    (t/testing "cons yields a list with element prepended"
      (t/is (= '(:a :a 1 :b 2 :c 3) (cons :a s))))))

(t/deftest object-features
  (let [s (bset/set 'a 1 :b 2)]
    (t/is (= "[a 1 :b 2]" (str s)))))

;; (t/deftest print-and-read-ordered
;;   (let [s (bset/set 1 2 9 8 7 5)]
;;     (t/is (= "#bestellt/set [1 2 9 8 7 5]"
;;            (pr-str s)))
;;     #_(let [o (read-string (pr-str s))]
;;       ;; #?(:clj (t/is (= bestellt.set.LinkedSet (type o))))
;;       #_(t/is (= '(1 2 9 8 7 5) (seq o))))))

(t/deftest comparing
  (let [s1 (bset/set 1 2 3)
        s2 (bset/set 1 2 4)]
    (t/testing "Comparable support"
     (t/is (= -1 (compare s1 s2)))
     (t/is (= 1 (compare s2 s1)))
     (t/is (= 0 (compare s1 s1))))))

(t/deftest flattening
  (let [s (bset/set 1 2 3)]
    (t/testing "flatten support"
     (t/is (= '(1 2 3 4 5 6)
              (flatten [s 4 5 6]))))))
