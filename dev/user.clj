(ns user
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.test :as test]
   [clojure.tools.namespace.repl :as r]
   [clojure.walk :refer [macroexpand-all]]
   [linked.core :as linked]
   [bestellt.map :as bmap]
   [criterium.core :refer [quick-bench bench with-progress-reporting]]))

(defmacro run-quick-bench
  [& exprs]
  `(with-progress-reporting (quick-bench (do ~@exprs) :verbose)))

(defmacro run-quick-bench'
  [& exprs]
  `(quick-bench (do ~@exprs)))

(defmacro run-bench
  [& exprs]
  `(with-progress-reporting (bench (do ~@exprs) :verbose)))

(defmacro run-bench'
  [& exprs]
  `(bench (do ~@exprs)))

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

(defn generate-map
  [m n]
  (reduce (fn [res i]
            (assoc res (keyword (str "a" i)) i))
          m
          (range n)))

(defn bench-assoc-big
  []
  (let [m1 (-> (linked/map) (generate-map 10))
        m2 (-> (bmap/map) (generate-map 10))
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
        m2 (-> (bmap/map) (generate-map 3))
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
        m2 (-> (bmap/map) (generate-map 10))
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
        m2 (-> (bmap/map) (generate-map 4))
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
        m2a (-> (bmap/map) (generate-map 10))
        m2b (-> (bmap/map) (generate-map 10))
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
        m2a (-> (bmap/map) (generate-map 4))
        m2b (-> (bmap/map) (generate-map 4))
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
