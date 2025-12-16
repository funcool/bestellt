(ns bestellt.set
  (:refer-clojure :exclude [set])
  (:require
   [bestellt.map :refer [empty-map]]
   [clojure.string :as string]
   #?(:cljs [cljs.reader :as reader]))
  #?(:clj
     (:import (clojure.lang Counted
                            IObj
                            IFn
                            IHashEq
                            ILookup
                            IPersistentCollection
                            IPersistentSet
                            IPersistentVector
                            Reversible
                            Seqable
                            Sequential
                            SeqIterator)
              (java.util Set)
              (java.lang Iterable))))

(declare empty-set)

#?(:clj
   (deftype LinkedSet [delegate]
     IPersistentSet
     (disjoin [_ k]
       (LinkedSet. (dissoc delegate k)))
     (contains [_ k]
       (contains? delegate k))
     (get [this k]
       (when (.contains this k) k))

     Set
     (size [this]
       (.count this))

     Iterable
     (iterator [this]
       (SeqIterator. (.seq this)))

     Comparable
     (compareTo [this o]
       (compare (vec this) (vec o)))


     Counted

     IPersistentCollection
     (count [_]
       (.count ^IPersistentCollection delegate))
     (cons [this o]
       (if (contains? delegate o)
         this
         (LinkedSet. (assoc delegate o nil))))
     (empty [_]
       empty-set)
     (equiv [this other]
       (or (identical? this other)
           (and (instance? Set other)
                (let [^Set s other]
                  (and (= (.size this) (.size s))
                       (every? #(.contains s %) (.seq this)))))))
     Sequential
     Seqable
     (seq [_]
       (when-let [s (seq delegate)]
         (map key s)))

     Reversible
     (rseq [_]
       (when-let [s (rseq delegate)]
         (map key s)))

     IFn
     (invoke [this k]
       (get this k))

     IObj
     (meta [this]
       (.meta ^IObj delegate))
     (withMeta [this m]
       (LinkedSet. (.withMeta ^IObj delegate m)))

     IHashEq
     (hasheq [this]
       ;; FIXME: revisit
       (.hasheq ^IHashEq (into #{} this)))

     Object
     (toString [this]
       (str "[" (string/join " " (map str this)) "]"))
     (hashCode [this]
       (.hashCode ^Object (into #{} this)))
     (equals [this other]
       (.equiv this other)))

   :cljs
   (deftype LinkedSet [delegate]
     Object
     (toString [this]
       (str "[" (string/join " " (map str this)) "]"))
     (equiv [this other]
       (-equiv this other))

     ICloneable
     (-clone [_] (LinkedSet. delegate))

     IWithMeta
     (-with-meta [coll meta] (LinkedSet. (with-meta delegate meta)))

     IMeta
     (-meta [coll] (meta delegate))

     ICollection
     (-conj [coll o]
       (LinkedSet. (assoc delegate o nil)))

     IEmptyableCollection
     (-empty [coll] (with-meta empty-set meta))

     IEquiv
     (-equiv [coll other]
       (and
        (set? other)
        (== (count coll) (count other))
        (every? #(contains? coll %)
                other)))

     IHash
     (-hash [coll] (hash (into #{} coll)))

     ISequential
     ISeqable
     (-seq [coll] (when-let [s (seq delegate)] (map key s)))

     IReversible
     (-rseq [coll] (when-let [s (rseq delegate)] (map key s)))

     ICounted
     (-count [coll] (-count delegate))

     IComparable
     (-compare [_ o]
       (compare (vec _) (vec o)))

     ILookup
     (-lookup [coll v]
       (-lookup coll v nil))
     (-lookup [coll v not-found]
       (if (-contains-key? delegate v)
         v
         not-found))

     ISet
     (-disjoin [coll v]
       (LinkedSet. (-dissoc delegate v)))

     IFn
     (-invoke [coll k]
       (-lookup coll k))
     (-invoke [coll k not-found]
       (-lookup coll k not-found))

     ;; IEditableCollection

     IPrintWithWriter
     (-pr-writer [coll writer opts]
       (-write writer (str "#linked/set " (into [] coll))))))

#?(:clj
   (defmethod print-method LinkedSet [o ^java.io.Writer w]
     (.write w "#bestellt/set ")
     (print-method (into [] o) w)))

(def ^{:tag LinkedSet} empty-set
  (LinkedSet. empty-map))

(def ->set
  #(into empty-set %))

(def set
  #(into empty-set %&))

#?(:cljs (reader/register-tag-parser! 'bestellt/set ->set))
