(ns bestellt.map
  (:refer-clojure :exclude [map map?])
  (:require [clojure.string :as string]
            [clojure.core :as c]
            #?(:cljs [cljs.reader :as reader]))
  #?(:clj
     (:import (clojure.lang Associative
                            Counted
                            IObj
                            IFn
                            IHashEq
                            ILookup
                            Util
                            IPersistentCollection
                            IPersistentVector
                            IPersistentMap
                            IMapEntry
                            MapEntry
                            MapEquivalence
                            Reversible
                            Seqable
                            SeqIterator)
              (java.util Map
                         Map$Entry
                         LinkedHashSet)
              (java.lang Iterable))))

#?(:clj (set! *warn-on-reflection* true))

(declare empty-map)

(deftype Node [k v l r]
  IMapEntry
  (key [_] k)
  (val [_] v)

  (getKey [_] k)
  (getValue [_] v)

  Object
  (equals [_ other]
    (cond
      (instance? Map$Entry other)
      (and (= k (.getKey ^Map$Entry other))
           (= v (.getValue ^Map$Entry other)))

      (instance? IPersistentVector other)
      (and (= 2 (.count ^IPersistentVector other))
           (= k (.nth ^IPersistentVector other 0))
           (= v (.nth ^IPersistentVector other 1)))

      :else
      false))

  clojure.lang.Seqable
  (seq [_]
    (list k v))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (if (int? k)
      (.nth this k not-found)
      (case k
        :k k
        :l l
        :r r
        :v v
        not-found)))

  clojure.lang.Sequential
  clojure.lang.Indexed
  (nth [_ index]
    (case index
      0 k
      1 v
      (throw (IndexOutOfBoundsException.))))

  (nth [_ index not-found]
    (case index
      0 k
      1 v
      not-found))

  clojure.lang.Counted
  (count [_] 2))

(declare assoc*)
(declare dissoc*)
(declare seq*)
(declare rseq*)

(deftype LinkedMap [head delegate]
  #?@(:clj
      [IPersistentMap
       (assoc [this k v]
              (assoc* this k v))
       (assocEx [this k v]
                (if (.containsKey this k)
                  (throw (RuntimeException. "Key already present"))
                  (assoc this k v)))
       (without [this k] (dissoc* this k))

       MapEquivalence

       Map
       (get [this k] (.valAt this k))
       (isEmpty [this] (not (.seq this)))
       (entrySet [this] (LinkedHashSet. (or (.seq this) [])))
       (containsValue [this v] (boolean (seq (filter #(= % v) (.values this)))))
       (values [this] (c/map val (.seq this)))
       (size [_] (count delegate))

       Counted
       IPersistentCollection
       (count [this] (.size this))
       (cons [this o]
             (condp instance? o
               Map$Entry (let [^Map$Entry e o]
                           (.assoc this (.getKey e) (.getValue e)))
               IPersistentVector (if (= 2 (count o))
                                   (.assoc this (nth o 0) (nth o 1))
                                   (throw (IllegalArgumentException. "Vector arg to map conj must be a pair")))
               ;; TODO support for transient to speed up multiple assoc?
               (reduce (fn [^IPersistentMap m ^Map$Entry e]
                         (.assoc m (.getKey e) (.getValue e)))
                       this
                       o)))
       (empty [_] (with-meta empty-map (meta delegate)))

       (equiv [this o]
              (and (instance? Map o)
                   (= (.count this) (count o))
                   (every? (fn [kv]
                             (let [k     (key kv)
                                   v     (val kv)
                                   other (find o k)]
                               (and (some? other)
                                    (= k (key other))
                                    (= v (val other)))))
                           (.seq this))))


       ;; clojure.lang.IReduce
       ;; (reduce [this f]
       ;;         (case (.count ^Counted this)
       ;;           0 (f)
       ;;           1 (get delegate head)
       ;;           2 (reduce* delegate f

       ;; (reduce [this f init]
       ;;         (let [head-node (get delegate head)
       ;;               last      (.-l ^Node head-node)]
       ;;           (reduce* delegate f head last init)))

       Seqable
       (seq [this]
            (seq* this))

       Reversible
       (rseq [this] (rseq* this))

       Iterable
       (iterator [this] (SeqIterator. (.seq this)))

       Associative
       (containsKey [_ k] (contains? delegate k))
       (entryAt [this k] (.valAt ^IPersistentMap delegate k))

       ILookup
       (valAt [this k]
              (.valAt this k nil))
       (valAt [_ k not-found]
              (if-let [entry (.valAt ^Associative delegate k)]
                (.-v ^Node entry)
                not-found))

       IFn
       (invoke [this k]
               (.valAt this k))
       (invoke [this k not-found]
               (.valAt this k not-found))

       IObj
       (meta [this]
             (.meta ^IObj delegate))
       (withMeta [this m]
                 (LinkedMap. head (.withMeta ^IObj delegate m)))

       ;; IEditableCollection

       IHashEq
       (hasheq [this]
               ;; TODO: cache hash value
               (.hasheq ^IHashEq (into {} this)))

       Object
       (toString [this]
                 (str "{" (string/join ", " (for [[k v] this] (str k " " v))) "}"))
       (equals [this other]
               (.equiv this other))
       (hashCode [this]
                 (.hashCode ^Object (into {} this)))]
      :cljs
      [Object
       (toString [coll]
                 (str "{" (string/join ", " (for [[k v] coll] (str k " " v))) "}"))
       (equiv [this other]
              (-equiv this other))

       ICloneable
       (-clone [_]
               (LinkedMap. head delegate))

       IWithMeta
       (-with-meta [coll meta]
                   (LinkedMap. head (with-meta delegate meta)))

       IMeta
       (-meta [coll] (meta delegate))

       ICollection
       (-conj [coll entry]
              (if (vector? entry)
                (-assoc coll (-nth entry 0) (-nth entry 1))
                (loop [ret coll es (seq entry)]
                  (if (nil? es)
                    ret
                    (let [e (first es)]
                      (if (vector? e)
                        (recur (-assoc ret (-nth e 0) (-nth e 1))
                               (next es))
                        (throw (js/Error. "conj on a map takes map entries or seqables of map entries"))))))))

       IEmptyableCollection
       (-empty [coll] (-with-meta empty-map (meta delegate)))

       IEquiv
       (-equiv [coll other] (equiv-map coll other))

       IHash
       (-hash [coll] (hash (into {} coll)))

       ISequential

       ISeqable
       (-seq [coll] (seq* coll))

       IReversible
       (-rseq [coll] (rseq* coll))

       ICounted
       (-count [coll]
               (count delegate))

       ILookup
       (-lookup [coll k]
                (-lookup coll k nil))

       (-lookup [coll k not-found]
                (if-let [node (c/-lookup delegate k)]
                  (.-v node)
                  not-found))

       ;; IFind
       ;; (-find [coll k]
       ;;        (if-let [node (c/-lookup delegate k)]
       ;;          node
       ;;          not-found))

       IAssociative
       (-assoc [coll k v]
               (assoc* coll k v))

       (-contains-key? [coll k]
                       (contains? delegate k))

       IMap
       (-dissoc [coll k]
                (dissoc* coll k))

       ;; ;; TODO: optimize / fix
       ;; IKVReduce
       ;; (-kv-reduce [coll f init]
       ;;             (reduce #(do
       ;;                        (prn "reduce" %2)
       ;;                        (apply (partial f %1) %2) init (seq coll))))

       IFn
       (-invoke [coll k]
                (-lookup coll k))

       (-invoke [coll k not-found]
                (-lookup coll k not-found))

       ;; IEditableCollection

       IPrintWithWriter
       (-pr-writer [coll writer opts] (-write writer (str "#bestellt/map " (into [] coll))))]))

(def xf:map-node-to-vec
  (c/map (fn [node] [(key node) (val node)])))

#?(:clj
   (defmethod print-method LinkedMap [o ^java.io.Writer w]
     (.write w "#bestellt/map ")
     (.write w (pr-str (into [] xf:map-node-to-vec o)))))

;; #?(:clj
;;    (defmethod print-method Node [o ^java.io.Writer w]
;;      (.write w (pr-str [(key o) (val o)]))))


(defn- update-node-value
  [^Node node v]
  (Node. (.-k node) v
         (.-l node)
         (.-r node)))

(defn- update-node-right
  [^Node node r]
  (Node. (.-k node)
         (.-v node)
         (.-l node)
         r))

(defn- update-node-left
  [^Node node l]
  (Node. (.-k node)
         (.-v node)
         l
         (.-r node)))

(defn- assoc*
  [^LinkedMap this k v]
  (let [head     (.-head this)
        delegate (.-delegate this)]
    (if (contains? delegate k)
      (LinkedMap. head (update delegate k update-node-value v))
      (if (empty? delegate)
        (LinkedMap. k (assoc delegate k (Node. k v k k)))
        (let [head-node (.valAt ^IPersistentMap delegate head)
              tail      (.-l ^Node head-node)]
          (LinkedMap. head (-> delegate
                               (assoc k (Node. k v tail head))
                               (update head update-node-left k)
                               (update tail update-node-right k))))))))

(defn- dissoc*
  [^LinkedMap this k]
  (let [head     (.-head this)
        delegate (.-delegate this)]
    (if-let [entry (.valAt ^IPersistentMap delegate k)]
      (if (= 1 (.count ^IPersistentMap delegate))
        (.empty this)
        (let [rk   (.-r ^Node entry)
              lk   (.-l ^Node entry)
              head (if (= k head) rk head)]
          (LinkedMap. head (-> delegate
                               (dissoc k)
                               (update rk update-node-left lk)
                               (update lk update-node-right rk)))))
      this)))


;;;; reduce

(defn- reduce*
  [delegate f fkey lkey result]
  (loop [current fkey
         result  result]
    (let [entry  (get delegate current)
          result (f result entry)]
      (if (= current lkey)
        result
        (recur (.-r ^Node entry) result)))))

;;;; seq and rseq impl

(defn- visit-node
  [delegate current last ^long direction]
  (let [entry (get delegate current)
        next  (case direction
                0 (.-l ^Node entry)
                1 (.-r ^Node entry))]
    (if (= current last)
      (list entry)
      (cons entry (lazy-seq (visit-node delegate next last direction))))))

(defn- seq*
  [^LinkedMap this]
  (let [delegate  (.-delegate this)
        head      (.-head this)
        head-node (get delegate head)]
    (when (pos? (.count ^IPersistentMap delegate))
      (let [tail (.-l ^Node head-node)]
        (visit-node delegate head tail 1)))))

(defn- rseq*
  [^LinkedMap this]
  (let [delegate (.-delegate this)
        head     (.-head this)
        tail     (-> delegate (get head) (get :l))]
    (when (seq delegate)
      (visit-node delegate tail head 0))))

(def empty-map
  (LinkedMap. nil {}))

(defn ->map
  [o]
  (into (LinkedMap. nil {}) o))

(defn map
  ([] empty-map)
  ([& kvpairs] (apply assoc empty-map kvpairs)))

(defn map?
  [o]
  (instance? LinkedMap o))

#?(:cljs (reader/register-tag-parser! 'bestellt/map ->map))
