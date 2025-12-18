(ns user
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.test :as test]
   [clojure.tools.namespace.repl :as r]
   [clojure.walk :refer [macroexpand-all]]
   [linked.core :as linked]
   [linked.map :as lkm]
   [bestellt.map :as bm]
   [bestellt.set :as bset]
   [criterium.core :refer [quick-bench bench with-progress-reporting]]))

(defmacro run-quick-bench
  [& exprs]
  `(with-progress-reporting (quick-bench (do ~@exprs) :verbose)))

(defmacro run-quick-bench'
  [& exprs]
  `(quick-bench (do ~@exprs) :verbose))

(defmacro run-bench
  [& exprs]
  `(with-progress-reporting (bench (do ~@exprs) :verbose)))

(defn- run-test
  ([] (run-test #"^bestellt.*-test$"))
  ([o]
   (r/refresh)
   (cond
     (instance? java.util.regex.Pattern o)
     (test/run-all-tests o)

     (symbol? o)
     (if-let [sns (namespace o)]
       (do (require (symbol sns))
           (test/test-vars [(resolve o)]))
       (test/test-ns o)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPERS AND DATA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate-map
  [m n]
  (reduce (fn [res i]
            (assoc res (keyword (str "a" i)) i))
          m
          (range n)))

(def sample
  (generate-map bm/empty-map 10))

(def sample-linked
  (generate-map (linked/map) 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BENCHMARKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bench-reduce-kv
  []
  (let [m1 (-> (linked/map) (generate-map 10))
        m2 (-> (bm/map) (generate-map 10))
        m3 (generate-map {} 10)]

    (println "=> linked")
    (run-quick-bench'
     (reduce-kv (fn [a k v] (+ a v)) 0 m1))

    (println "=> bestellt")
    (run-quick-bench'
     (reduce-kv (fn [a k v] (+ a v)) 0 m2))

    (println "=> native")
    (run-quick-bench'
     (reduce-kv (fn [a k v] (+ a v)) 0 m3))))

(defn bench-assoc-big
  []
  (let [m1 (-> (linked/map) (generate-map 10))
        m2 (-> (bm/map) (generate-map 10))
        m3 (generate-map {} 10)]

    (println "=> linked")
    (run-quick-bench'
     (-> m1
         (assoc :c1 1)
         (assoc :c2 2)
         (assoc :c3 3)
         (assoc :c4 4)
         (assoc :c5 5)))

    (println "=> bestellt")
    (run-quick-bench'
     (-> m2
         (assoc :c1 1)
         (assoc :c2 2)
         (assoc :c3 3)
         (assoc :c4 4)
         (assoc :c5 5)))

    (println "=> native")
    (run-quick-bench'
     (-> m3
         (assoc :c1 1)
         (assoc :c2 2)
         (assoc :c3 3)
         (assoc :c4 4)
         (assoc :c5 5)))))

(defn bench-assoc-small
  []
  (let [m1 (-> (linked/map) (generate-map 3))
        m2 (-> (bm/map) (generate-map 3))
        m3 (generate-map {} 3)]

    (println "=> linked")
    (run-quick-bench'
     (-> m1
         (assoc :c1 1)
         (assoc :c2 2)
         (assoc :c3 3)))

    (println "=> bestellt")
    (run-quick-bench'
     (-> m2
         (assoc :c1 1)
         (assoc :c2 2)
         (assoc :c3 3)))

    (println "=> native")
    (run-quick-bench'
     (-> m3
         (assoc :c1 1)
         (assoc :c2 2)
         (assoc :c3 3)))))

(defn bench-seq-big
  []
  (let [m1 (-> (linked/map) (generate-map 10))
        m2 (-> (bm/map) (generate-map 10))
        m3 (generate-map {} 10)]

    (println "=> linked")
    (run-quick-bench'
     (doall (seq m1)))

    (println "=> bestellt")
    (run-quick-bench'
     (doall (seq m2)))

    (println "=> native")
    (run-quick-bench'
     (doall (seq m3)))))

(defn bench-seq-small
  []
  (let [m1 (-> (linked/map) (generate-map 4))
        m2 (-> (bm/map) (generate-map 4))
        m3 (generate-map {} 4)]

    (println "=> linked")
    (run-quick-bench'
     (doall (seq m1)))

    (println "=> bestellt")
    (run-quick-bench'
     (doall (seq m2)))

    (println "=> native")
    (run-quick-bench'
     (doall (seq m3)))))

(defn bench-equality-big
  []
  (let [m1a (-> (linked/map) (generate-map 10))
        m1b (-> (linked/map) (generate-map 10))
        m2a (-> (bm/map) (generate-map 10))
        m2b (-> (bm/map) (generate-map 10))
        m3a (generate-map {} 10)
        m3b (generate-map {} 10)]

    (println "=> linked")
    (run-quick-bench'
     (= m1a m1b))

    (println "=> bestellt")
    (run-quick-bench'
     (= m2a m2b))

    (println "=> native")
    (run-quick-bench'
     (= m3a m3b))))

(defn bench-equality-small
  []
  (let [m1a (-> (linked/map) (generate-map 4))
        m1b (-> (linked/map) (generate-map 4))
        m2a (-> (bm/map) (generate-map 4))
        m2b (-> (bm/map) (generate-map 4))
        m3a (generate-map {} 4)
        m3b (generate-map {} 4)]

    (println "=> linked")
    (run-quick-bench'
     (= m1a m1b))

    (println "=> bestellt")
    (run-quick-bench'
     (= m2a m2b))

    (println "=> native")
    (run-quick-bench'
     (= m3a m3b))))

(defn- index-of-pred
  [coll pred]
  (loop [c    (first coll)
         coll (rest coll)
         index 0]
    (if (nil? c)
      nil
      (if (pred c)
        index
        (recur (first coll)
               (rest coll)
               (inc index))))))

(defn- index-of
  [coll v]
  (index-of-pred coll #(= % v)))

(defn- oassoc
  [o & kvs]
  (apply assoc (or o (linked/map)) kvs))

(defn- oupdate-in
  [m ks f & args]
  (let [up (fn up [m ks f args]
             (let [[k & ks] ks]
               (if ks
                 (oassoc m k (up (get m k) ks f args))
                 (oassoc m k (apply f (get m k) args)))))]
    (up m ks f args)))

(defn assoc-before [m before-k k v]
  (if-not (contains? m before-k)
    (assoc m k v)
    (let [m' (dissoc m k)]
      (reduce-kv
        (fn [acc kk vv]
          (if (= kk before-k)
            (-> acc
                (assoc k v)
                (assoc kk vv))
            (assoc acc kk vv)))
        (linked/map)
        m'))))

(defn bench-assoc-before
  []
  (let [m1 (-> (linked/map) (generate-map 10))
        m2 (-> (bm/map) (generate-map 10))]
    (println "=> linked")
    (run-quick-bench'
     (assoc-before3 m2 :a0 :a9 -1))

    (println "=> bestellt")
    (run-quick-bench'
     (bm/assoc-before m2 :a0 :a9 -1))))

(defn bench-update-in
  []
  (println "=> linked")
  (run-quick-bench'
   (oupdate-in nil [:a :b :c] oassoc :d 2))

  (println "=> bestellt")
  (run-quick-bench'
   (bm/update-in nil [:a :b :c] bm/assoc :d 2)))





