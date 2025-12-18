(ns bestellt.map-test
  (:require
   [bestellt.map :as bmap]
   [clojure.test :as t]
   [clojure.test.check.generators :as gen]
   #?(:clj [collection-check.core :as ct])
   #?(:cljs [cljs.reader :refer [read-string]])))

#?(:clj
   (t/deftest check-1
     (ct/assert-map-like (bmap/map) gen/int gen/int)))

#?(:clj
   (t/deftest implementations
     (let [basic (bmap/map)
           other (bmap/map 1 2)
           node  (first other)]

       (t/are [class] (instance? class basic)
         clojure.lang.IPersistentMap
         clojure.lang.IPersistentCollection
         clojure.lang.Counted
         clojure.lang.Associative
         java.util.Map)

       (t/are [class] (instance? class node)
         clojure.lang.IHashEq
         clojure.lang.Seqable
         clojure.lang.ILookup
         clojure.lang.Sequential
         clojure.lang.Indexed
         clojure.lang.Counted
         clojure.lang.IReduceInit
         java.lang.Iterable)

       (t/are [object] (= (class object) (class basic))
         (conj basic [1 2])
         (assoc basic 1 2)
         (into basic {1 2}))

       (t/are [object] (nil? object)
         (seq basic)
         (rseq basic)))))

(t/deftest reduce-1
  (let [data   (bmap/map :a 1 :b 2 :c 3)
        result (reduce conj [] data)]
    (t/is (= (seq result) (seq data)))))

(t/deftest reduce-2
  (let [data   (bmap/map :a 1 :b 2 :c 3)
        result (reduce (fn [a node] (+ a (val node))) 0 data)]
    (t/is (= 6 result))))

(t/deftest reduce-empty
  (let [data   (bmap/map)
        result (reduce conj [] data)]
    (t/is (= (seq result) (seq data)))))

(t/deftest reduce-kv-empty
  (let [data   (bmap/map)
        result (reduce-kv (fn [s k v] (+ s v)) 0 data)]
    (t/is (zero? result))))

(t/deftest reduce-kv-1
  (let [data   (bmap/map :a 1 :b 2 :c 3)
        result (reduce-kv (fn [s k v] (+ s v)) 0 data)]
    (t/is (= 6 result))))

(t/deftest find-1
  (let [data   (bmap/map :a 1 :b 2 :c 3)
        result (find data :a)]
    (t/is #?(:clj (instance? clojure.lang.IMapEntry result)
             :cljs (implements? cljs.core/IMapEntry result)))))

(t/deftest equality
  (let [empty-map bmap/empty-map
        one-item  (assoc empty-map 1 2)]

    (t/testing "basic symmetric equality"
      (t/is (= {} empty-map))
      (t/is (= empty-map {}))
      (t/is (= {1 2} one-item))
      (t/is (= one-item {1 2})))

    (t/testing "order-insensitive comparisons"
      (let [one-way   (into empty-map {1 2 3 4})
            other-way (into empty-map {3 4 1 2})
            unsorted  {1 2 3 4}]
        (t/is (= one-way other-way))
        (t/is (= one-way unsorted))
        (t/is (= other-way unsorted))))

    (t/testing "hash code sanity"
      (t/is (integer? (hash one-item)))
      (t/is (= (hash {1 2}) (hash one-item))))

    (t/testing "does not blow up when give something different"
      (t/is (not= one-item 'baz))
      (t/is (not= 'baz one-item)))

    (t/testing "nil values don't break .equiv"
      (t/is (not= (bmap/map :x nil) {:y 0})))))

(t/deftest ordering
  (let [values [[:first 10]
                [:second 20]
                [:third 30]]
        m (into (bmap/map) values)]

    (t/testing "seq behaves like on a seq of vectors"
      (t/is (= (seq values) (seq m))))

    (t/testing "new values get added at the end"
      (let [entry [:fourth 40]]
        (t/is (= (seq (conj values entry))
               (seq (conj m entry))))))

    (t/testing "vhanging old mappings leaves them at the same location"
      (let [vec-index [1]
            vec-key   (conj vec-index 1)
            map-key   (get-in values (conj vec-index 0))
            new-value 5]
        (t/is (= (seq (assoc-in values vec-key new-value))
                 (seq (assoc m map-key new-value))))))

    (t/testing "large number of keys still sorted"
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
    (t/testing "keyword lookup"
      (t/is (= 1 (:a m))))
    (t/testing "dequence views"
      (t/is (= [:a :b :c] (keys m)))
      (t/is (= [1 2 3] (vals m))))
    (t/testing "IFn support"
      (t/is (= 2 (m :b)))
      (t/is (= 'not-here (m :nothing 'not-here)))
      (t/is (= nil ((bmap/map :x nil) :x 'not-here))))
    (t/testing "get out Map.Entry"
      (t/is (= [:a 1] (find m :a))))
    (t/testing "get out Map.Entry with falsy value"
      (t/is (= [:a nil] (find (bmap/map :a nil) :a))))
    (t/testing "ordered dissoc"
      (let [m (dissoc m :b)]
        (t/is (= [:a :c] (keys m)))
        (t/is (= [1 3] (vals m)))))
    (t/testing "empty equality"
      (let [m (dissoc m :b :a :c)]
        (t/is (= (bmap/map) m))))
    (t/testing "can conj a map"
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

(t/deftest map-entry-test
  (t/is (map-entry? (first (bmap/map 1 2)))))

#?(:clj
   (t/deftest java-interop
     (t/is (.isEmpty ^java.util.Map (bmap/map)))
     (t/is (not (.isEmpty ^java.util.Map (bmap/map 1 2))))
     (t/is (= [] (vec (.entrySet ^java.util.Map (bmap/map)))))
     (t/is (= (partition 2 (range 100)) (vec (.entrySet ^java.util.Map (apply bmap/map (range 100))))))))

(t/deftest transient-1
  (let [result (-> bmap/empty-map
                   (assoc -4 4)
                   (dissoc -2)
                   (transient)
                   (assoc! 0 2)
                   (assoc! 1 3)
                   (dissoc! -4)
                   (persistent!))]
    (t/is (= result (into bmap/empty-map [[0 2] [1 3]])))))

(t/deftest transient-2
  (let [result (-> bmap/empty-map
                   (assoc :a 1)
                   (transient)
                   (dissoc! :a)
                   (persistent!))]
    (t/is (= result bmap/empty-map))))

(t/deftest rename-key-1
  (let [data   (-> bmap/empty-map
                 (assoc :a 1)
                 (assoc :b 2))
        result (bmap/rename-key data :a :c)]
    (t/is (= 2 (count data)))
    (t/is (= 2 (count result)))
    (let [[kp1 kp2] (seq result)]
      (t/is (= :c (key kp1)))
      (t/is (= :b (key kp2)))
      (t/is (= 1 (val kp1)))
      (t/is (= 2 (val kp2))))))

(t/deftest rename-key-2
  (let [data   (-> bmap/empty-map
                 (assoc :a 1)
                 (assoc :b 2))
        result (bmap/rename-key data :c :d)]
    (t/is (identical? result data))))

(t/deftest rename-key-3
  (let [data    bmap/empty-map
        result (bmap/rename-key data :c :d)]
    (t/is (identical? result data))))

(t/deftest assoc-after-1
  (let [data   (-> bmap/empty-map
                   (assoc :a 1)
                   (assoc :b 2))
        result (bmap/assoc-after data :a :c 0)]
    (t/is (= 2 (count data)))
    (t/is (= 3 (count result)))
    (t/is (= [:a :c :b] (keys result)))
    (t/is (= [1 0 2] (vals result)))))

(t/deftest assoc-after-2
  (let [data   (-> bmap/empty-map
                   (assoc :a 1)
                   (assoc :b 2))
        result (bmap/assoc-after data nil :c 0)]
    (t/is (= 2 (count data)))
    (t/is (= 3 (count result)))
    (t/is (= [:c :a :b] (keys result)))
    (t/is (= [0 1 2] (vals result)))))

(t/deftest assoc-after-3
  (let [data   (-> bmap/empty-map
                   (assoc :a 1)
                   (assoc :b 2))
        result (bmap/assoc-after data :b :c 0)]
    (t/is (= 2 (count data)))
    (t/is (= 3 (count result)))
    (t/is (= [:a :b :c] (keys result)))
    (t/is (= [1 2 0] (vals result)))))


(t/deftest assoc-after-4
  (let [data   (-> bmap/empty-map
                   (assoc :a 1)
                   (assoc :b 2)
                   (assoc :c 3))
        result (bmap/assoc-after data nil :c 0)]
    (t/is (= 3 (count data)))
    (t/is (= 3 (count result)))
    (t/is (= [:c :a :b] (keys result)))
    (t/is (= [0 1 2] (vals result)))))

(t/deftest assoc-before-1
  (let [data   (-> bmap/empty-map
                   (assoc :a 1)
                   (assoc :b 2))
        result (bmap/assoc-before data :a :c 0)]
    (t/is (= 2 (count data)))
    (t/is (= 3 (count result)))
    (t/is (= [:c :a :b] (keys result)))
    (t/is (= [0 1 2] (vals result)))))

(t/deftest assoc-before-2
  (let [data   (-> bmap/empty-map
                   (assoc :a 1)
                   (assoc :b 2))
        result (bmap/assoc-before data nil :c 0)]
    (t/is (= 2 (count data)))
    (t/is (= 3 (count result)))
    (t/is (= [:a :b :c] (keys result)))
    (t/is (= [1 2 0] (vals result)))))

(t/deftest assoc-before-3
  (let [data   (-> bmap/empty-map
                   (assoc :a 1)
                   (assoc :b 2))
        result (bmap/assoc-before data :b :c 0)]
    (t/is (= 2 (count data)))
    (t/is (= 3 (count result)))
    (t/is (= [:a :c :b] (keys result)))
    (t/is (= [1 0 2] (vals result)))))

