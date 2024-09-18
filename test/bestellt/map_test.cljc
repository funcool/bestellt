(ns bestellt.map-test
  (:require
   [bestellt.map :as bmap]
   #?(:clj  [clojure.test :as t]
      :cljs [cljs.test :as t ])
   #?(:cljs [cljs.reader :refer [read-string]])))

#?(:clj
   (t/deftest implementations
     (let [basic (bmap/map)]
       (t/are [class] (instance? class basic)
         clojure.lang.IPersistentMap
         clojure.lang.IPersistentCollection
         clojure.lang.Counted
         clojure.lang.Associative
         java.util.Map)

       (t/are [object] (= (class object) (class basic))
         (conj basic [1 2])
         (assoc basic 1 2)
         (into basic {1 2}))

       (t/are [object] (nil? object)
         (seq basic)
         (rseq basic)))))

(t/deftest equality
  (let [empty-map (bmap/map)
        one-item  (assoc empty-map 1 2)]

    (t/testing "Basic symmetric equality"
      (t/is (= {} empty-map))
      (t/is (= empty-map {}))
      (t/is (= {1 2} one-item))
      (t/is (= one-item {1 2})))

    (t/testing "Order-insensitive comparisons"
      (let [one-way   (into empty-map {1 2 3 4})
            other-way (into empty-map {3 4 1 2})
            unsorted  {1 2 3 4}]
        (t/is (= one-way other-way))
        (t/is (= one-way unsorted))
        (t/is (= other-way unsorted))))

    (t/testing "Hash code sanity"
      (t/is (integer? (hash one-item)))
      (t/is (= (hash {1 2}) (hash one-item))))

    (t/testing "Does not blow up when give something different"
      (t/is (not= one-item 'baz))
      (t/is (not= 'baz one-item)))

    (t/testing "nil values don't break .equiv"
      (t/is (not= (bmap/map :x nil) {:y 0})))))

(t/deftest ordering
  (let [values [[:first 10]
                [:second 20]
                [:third 30]]
        m (into (bmap/map) values)]

    (t/testing "Seq behaves like on a seq of vectors"
      (t/is (= (seq values) (seq m))))

    (t/testing "New values get added at the end"
      (let [entry [:fourth 40]]
        (t/is (= (seq (conj values entry))
               (seq (conj m entry))))))

    (t/testing "Changing old mappings leaves them at the same location"
      (let [vec-index [1]
            vec-key   (conj vec-index 1)
            map-key   (get-in values (conj vec-index 0))
            new-value 5]
        (t/is (= (seq (assoc-in values vec-key new-value))
                 (seq (assoc m map-key new-value))))))

    (t/testing "Large number of keys still sorted"
      (let [kvs (for [n (range 5000)]
                  [(str n) n])
            ordered (into m kvs)]
        (= (seq kvs) (seq ordered))))))

(t/deftest reversing
  (let [source (vec (for [n (range 10)]
                      [n n]))
        m (into (bmap/map) source)]
    (t/is (= (rseq m) (rseq source)))))

(t/deftest map-features
  (let [m (bmap/map :a 1 :b 2 :c 3)]
    (t/testing "Keyword lookup"
      (t/is (= 1 (:a m))))
    (t/testing "Sequence views"
      (t/is (= [:a :b :c] (keys m)))
      (t/is (= [1 2 3] (vals m))))
    (t/testing "IFn support"
      (t/is (= 2 (m :b)))
      (t/is (= 'not-here (m :nothing 'not-here)))
      (t/is (= nil ((bmap/map :x nil) :x 'not-here))))
    (t/testing "Get out Map.Entry"
      (t/is (= [:a 1] (find m :a))))
    (t/testing "Get out Map.Entry with falsy value"
      (t/is (= [:a nil] (find (bmap/map :a nil) :a))))
    (t/testing "Ordered dissoc"
      (let [m (dissoc m :b)]
        (t/is (= [:a :c] (keys m)))
        (t/is (= [1 3] (vals m)))))
    (t/testing "Empty equality"
      (let [m (dissoc m :b :a :c)]
        (t/is (= (bmap/map) m))))
    (t/testing "Can conj a map"
      (t/is (= {:a 1 :b 2 :c 3 :d 4} (conj m {:d 4}))))
    (t/testing "(conj m nil) returns m"
      (t/are [x] (= m x)
        (conj m nil)
        (merge m ())
        (into m ())))
    (t/testing  "meta support"
      (t/is (= {'a 'b} (meta (with-meta m {'a 'b})))))))

(t/deftest object-features
  (let [m (bmap/map 'a 1 :b 2)]
    (t/is (= "{a 1, :b 2}" (str m)))))

(t/deftest print-and-read-ordered
  (let [s (bmap/map 1 2, 3 4, 5 6, 1 9, 7 8)]
    (t/is (= "#bestellt/map [[1 9] [3 4] [5 6] [7 8]]"
             (pr-str s)))
    (let [o (read-string (pr-str s))]
      ;; #?(:clj (t/is (bmap/map? o)))
      (t/is (= '([1 9] [3 4] [5 6] [7 8])
             (seq o))))))

;; (t/deftest map-entry-test
;;   (t/is (map-entry? (first (bmap/map 1 2)))))

;; #?(:clj
;;    (t/deftest java-interop
;;      (t/is (.isEmpty ^java.util.Map (bmap/map)))
;;      (t/is (not (.isEmpty ^java.util.Map (bmap/map 1 2))))
;;      (t/is (= [] (vec (.entrySet ^java.util.Map (bmap/map)))))
;;      (t/is (= (partition 2 (range 100)) (vec (.entrySet ^java.util.Map (apply bmap/map (range 100))))))))
