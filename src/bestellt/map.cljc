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
                            Murmur3
                            Indexed
                            Sequential
                            Util
                            IPersistentCollection
                            IPersistentVector
                            IPersistentMap
                            IEditableCollection
                            ITransientMap
                            ITransientAssociative2
                            IMapEntry
                            IReduceInit
                            IReduce
                            IKVReduce
                            MapEntry
                            MapEquivalence
                            Reversible
                            Seqable
                            SeqIterator)
              (java.util Map
                         Map$Entry
                         Collection
                         LinkedHashSet)
              (java.lang Iterable))))

#?(:clj (set! *warn-on-reflection* true))

(declare empty-map)
(declare ^:private make-linked-map)
(declare ^:private make-linked-transient-map)
(declare ^:private equiv-sequential)

(defprotocol ILinkedMapInternal
  (^:no-doc -get-head [this] "get current head")
  (^:no-doc -get-delegate [this] "get current delegate"))

(defprotocol ILinkedMap
  "A protocol that defines specific API for map that conserves insertion order"
  (-assoc-after [_ key k v] "Assoc a new entry after an existing key")
  (-assoc-before [_ key k v] "Assoc a new entry before an existing key")
  (-rename-key [_ key k] "Rename a key of a entry respecting the existing order"))

#?(:clj
   (deftype Node [k v l r ^:unsynchronized-mutable _hash]
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

     IHashEq
     (hasheq [this]
       (when-not _hash
         (set! _hash (Murmur3/hashOrdered (.seq ^Seqable this))))
       _hash)

     Seqable
     (seq [_]
       (list k v))

     Iterable
     (iterator [this] (SeqIterator. (.seq this)))

     ILookup
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

     Sequential
     Indexed
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

     Counted
     (count [_] 2)

     ;; Just for faster to vec conversion, does not respects reduced
     IReduceInit
     (reduce [this f start]
       (-> start
           (f k)
           (f v))))

   :cljs
   (deftype Node [k v l r ^:mutable _hash]
     IEquiv
     (-equiv [coll other] (equiv-sequential coll other))

     IHash
     (-hash [coll] (caching-hash coll hash-ordered-coll _hash))

     IMapEntry
     (-key [node] k)
     (-val [node] v)

     ISequential
     ISeqable
     (-seq [coll] (list k v))

     ICounted
     (-count [_] 2)

     IIndexed
     (-nth [coll index]
       (case index
         0 k
         1 v
         (throw (js/Error. "Index out of bounds"))))

     (-nth [coll index not-found]
       (case index
         0 k
         1 v
         not-found))

     ILookup
     (-lookup [this k]
       (-lookup this k nil))

     (-lookup [this k not-found]
       (if (int? k)
         (-nth this k not-found)
         (case k
           :k k
           :l l
           :r r
           :v v
           not-found)))

     IPrintWithWriter
     (-pr-writer [coll writer opts] (-write writer (str "[" k " " v "]")))))

(declare ^:private assoc*)
(declare ^:private cons*)
(declare ^:private assoc-after*)
(declare ^:private assoc-before*)
(declare ^:private rename-key*)
(declare ^:private dissoc*)
(declare ^:private seq*)
(declare ^:private rseq*)
(declare ^:private reduce*)
(declare ^:private kvreduce*)

#?(:clj
   (deftype LinkedMap [head delegate ^:unsynchronized-mutable _hash]
     IPersistentMap
     (assoc [this k v]
       (assoc* this k v make-linked-map c/assoc))
     (assocEx [this k v]
       (if (.containsKey this k)
         (throw (RuntimeException. "Key already present"))
         (assoc this k v)))
     (without [this k]
       (dissoc* this k make-linked-map c/assoc c/dissoc))

     MapEquivalence

     ILinkedMap
     (-assoc-after [this key k v]
       (assoc-after* this key k v make-linked-map c/assoc c/dissoc))
     (-assoc-before [this key k v]
       (assoc-before* this key k v make-linked-map c/assoc c/dissoc))
     (-rename-key [this key k]
       (rename-key* this key k make-linked-map c/assoc c/dissoc))

     ILinkedMapInternal
     (-get-delegate [_] delegate)
     (-get-head [_] head)

     Map
     (get [this k]
       (.valAt this k))
     (isEmpty [this]
       (not (.seq this)))
     (entrySet [this]
       (let [coll (or (.seq this) (list))]
         (LinkedHashSet. ^Collection coll)))
     (containsValue [this v]
       (boolean (seq (filter #(= % v) (.values this)))))
     (values [this]
       (c/map val (.seq this)))
     (size [_]
       (count delegate))

     Counted
     IPersistentCollection

     (count [this]
       (.size this))

     (cons [this val]
       (cons* c/assoc this val))

     (empty [_]
       (with-meta empty-map (meta delegate)))

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

     IReduceInit
     IReduce
     (reduce [this f]
       (if (zero? (.count ^Counted this))
         (f)
         (let [head-node (get delegate head)
               next-head (.-r ^Node head-node)
               last      (.-l ^Node head-node)]
           (reduce* delegate f next-head last head-node))))

     (reduce [this f init]
       (if (zero? (.count ^Counted this))
         init
         (let [head-node (get delegate head)
               last      (.-l ^Node head-node)]
           (reduce* delegate f head last init))))

     IKVReduce
     (kvreduce [this f init]
       (kvreduce* delegate head f init))

     Seqable
     (seq [this]
       (seq* this))

     Reversible
     (rseq [this]
       (rseq* this))

     Iterable
     (iterator [this]
       (SeqIterator. (.seq this)))

     Associative
     (containsKey [_ k]
       (contains? delegate k))
     (entryAt [this k]
       (.valAt ^IPersistentMap delegate k))

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
       (LinkedMap. head (.withMeta ^IObj delegate m) nil))

     IEditableCollection
     (asTransient [this]
       (make-linked-transient-map head (transient delegate)))

     IHashEq
     (hasheq [this]
       (when-not _hash
         (set! _hash (clojure.lang.Murmur3/hashUnordered this)))
       _hash)

     Object
     (toString [this]
       (str "{" (string/join ", " (for [[k v] this] (str k " " v))) "}"))
     (equals [this other]
       (.equiv this other))
     (hashCode [this]
       (.hashCode ^Object (into {} this))))

   :cljs
   (deftype LinkedMap [head delegate ^:mutable _hash]
     Object
     (toString [coll]
       (str "{" (string/join ", " (for [[k v] coll] (str k " " v))) "}"))
     (equiv [this other]
       (-equiv this other))

     ILinkedMap
     (-assoc-after [this key k v]
       (assoc-after* this key k v make-linked-map c/assoc c/dissoc))
     (-assoc-before [this key k v]
       (assoc-before* this key k v make-linked-map c/assoc c/dissoc))
     (-rename-key [this key k]
       (rename-key* this key k make-linked-map c/assoc c/dissoc))

     ILinkedMapInternal
     (-get-delegate [_] delegate)
     (-get-head [_] head)

     ICloneable
     (-clone [_]
       (LinkedMap. head delegate nil))

     IWithMeta
     (-with-meta [coll meta]
       (LinkedMap. head (with-meta delegate meta) nil))

     IMeta
     (-meta [coll] (meta delegate))

     ICollection
     (-conj [this val]
       (cons* c/assoc this val))

     IEmptyableCollection
     (-empty [coll] (-with-meta empty-map (meta delegate)))

     IEquiv
     (-equiv [coll other]
       (equiv-map coll other))

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
         (.-v ^Node node)
         not-found))

     IFind
     (-find [coll k]
       (c/-lookup delegate k))

     IReduce
     (-reduce [this f]
       (if (zero? (count this))
         (f)
         (let [head-node (get delegate head)
               next-head (.-r ^Node head-node)
               last      (.-l ^Node head-node)]
           (reduce* delegate f next-head last head-node))))

     (-reduce [this f init]
       (if (zero? (count this))
         init
         (let [head-node (get delegate head)
               last      (.-l ^Node head-node)]
           (reduce* delegate f head last init))))

     IKVReduce
     (-kv-reduce [this f init]
       (kvreduce* delegate head f init))


     IAssociative
     (-assoc [coll k v]
       (assoc* coll k v make-linked-map c/assoc))

     (-contains-key? [coll k]
       (contains? delegate k))

     IMap
     (-dissoc [coll k]
       (dissoc* coll k make-linked-map c/assoc c/dissoc))

     IFn
     (-invoke [coll k]
       (-lookup coll k))

     (-invoke [coll k not-found]
       (-lookup coll k not-found))

     IEditableCollection
     (-as-transient [this]
       (make-linked-transient-map head (transient delegate)))

     IPrintWithWriter
     (-pr-writer [coll writer opts] (-write writer (str "#bestellt/map " (into [] coll))))))

#?(:clj
   (deftype TransientLinkedMap [head delegate ^:unsynchronized-mutable _hash]
     ITransientMap
     (assoc [this k v]
       (assoc* this k v make-linked-transient-map c/assoc!))
     (without [this k]
       (dissoc* this k make-linked-transient-map c/assoc! c/dissoc!))

     (persistent [this]
       (make-linked-map head (persistent! delegate)))
     (conj [this val]
       (cons* c/assoc! this val))

     (valAt [this k]
       (.valAt this k nil))
     (valAt [_ k not-found]
       (if-let [entry (.valAt ^Associative delegate k)]
         (.-v ^Node entry)
         not-found))

     (count [this]
       (count delegate))

     ILinkedMap
     (-assoc-after [this key k v]
       (assoc-after* this key k v make-linked-transient-map c/assoc! c/dissoc!))
     (-assoc-before [this key k v]
       (assoc-before* this key k v make-linked-transient-map c/assoc! c/dissoc!))
     (-rename-key [this key k]
       (rename-key* this key k make-linked-transient-map c/assoc! c/dissoc!))

     ILinkedMapInternal
     (-get-delegate [_] delegate)
     (-get-head [_] head)

     ITransientAssociative2
     (containsKey [_ k]
       (contains? delegate k))

     (entryAt [this k]
       (.valAt ^ITransientMap delegate k))

     IFn
     (invoke [this k]
       (.valAt this k))
     (invoke [this k not-found]
       (.valAt this k not-found)))

   :cljs
   (deftype TransientLinkedMap [head delegate ^:mutable _hash]
     ITransientMap
     (-dissoc! [this key]
       (dissoc* this key make-linked-transient-map c/assoc! c/dissoc!))

     ITransientAssociative
     (-assoc! [this key val]
       (assoc* this key val make-linked-transient-map c/assoc!))

     ITransientCollection
     (-conj! [this val]
       (cons* c/assoc! this val))
     (-persistent! [this]
       (make-linked-map head (persistent! delegate)))

     ILinkedMap
     (-assoc-after [this key k v]
       (assoc-after* this key k v make-linked-transient-map c/assoc! c/dissoc!))
     (-assoc-before [this key k v]
       (assoc-before* this key k v make-linked-transient-map c/assoc! c/dissoc!))
     (-rename-key [this key k]
       (rename-key* this key k make-linked-transient-map c/assoc! c/dissoc!))

     ILinkedMapInternal
     (-get-delegate [_] delegate)
     (-get-head [_] head)

     IFn
     (-invoke [this k]
       (-lookup this k))

     (-invoke [this k not-found]
       (-lookup this k not-found))

     ILookup
     (-lookup [coll k]
       (-lookup coll k nil))

     (-lookup [coll k not-found]
       (if-let [node (c/-lookup delegate k)]
         (.-v ^Node node)
         not-found))))

(def ^:private xf:map-node-to-vec
  (c/map (fn [node] [(key node) (val node)])))

#?(:clj
   (defmethod print-method LinkedMap [o ^java.io.Writer w]
     (.write w "#bestellt/map ")
     (.write w (pr-str (into [] xf:map-node-to-vec o)))))

#?(:clj
   (defmethod print-method Node [^Node o ^java.io.Writer w]
     (.write w (pr-str [(key o) (val o) :left (.-l o) :right (.-r o)]))))

#?(:clj
   (defmethod print-method Node
     [o w]
     (print-dup o w)))

#?(:clj
   (defmethod print-dup Node
     [o ^java.io.Writer writer]
     (.write writer (str "#bestellt/node [" (key o) " " (val o) "]"))))

(defn- equiv-sequential
  [x y]
  (boolean
   (when (sequential? y)
     (if (and (counted? x) (counted? y)
              (not (== (count x) (count y))))
       false
       (loop [xs (seq x) ys (seq y)]
          (cond (nil? xs) (nil? ys)
                (nil? ys) false
                (= (first xs) (first ys)) (recur (next xs) (next ys))
                :else false))))))

(defn- make-linked-map
  [head delegate]
  (LinkedMap. head delegate nil))

(defn- make-linked-transient-map
  [head delegate]
  (TransientLinkedMap. head delegate nil))

(defn- make-node
  [k v l r]
  (Node. k v l r nil))

(defn- update-node-key
  [^Node node k]
  (make-node
   k
   (.-v node)
   (.-l node)
   (.-r node)))

(defn- update-node-value
  [^Node node v]
  (make-node
   (.-k node)
   v
   (.-l node)
   (.-r node)))

(defn- update-node-right
  [^Node node r]
  (make-node
   (.-k node)
   (.-v node)
   (.-l node)
   r))

(defn- update-node-left
  [^Node node l]
  (make-node
   (.-k node)
   (.-v node)
   l
   (.-r node)))

(defn- update*
  "A specialized update function that uses provided assoc-fn instead the
  default one; This one allows reuse the same impl for persistent and transient
  data structures."
  [target assoc-fn k f param]
  (let [node (get target k)
        node (f node param)]
    (assoc-fn target k node)))

(defn- assoc*
  [this k v make-linked-map assoc-fn]
  (let [head     (-get-head this)
        delegate (-get-delegate this)]
    (if (contains? delegate k)
      (make-linked-map head (update* delegate assoc-fn k update-node-value v))
      (if (empty? delegate)
        (make-linked-map k (assoc-fn delegate k (make-node k v k k)))
        (let [head-node (get delegate head)
              tail      (.-l ^Node head-node)]
          (make-linked-map head
                           (-> delegate
                               (assoc-fn k (make-node k v tail head))
                               (update* assoc-fn head update-node-left k)
                               (update* assoc-fn tail update-node-right k))))))))

(defn- dissoc*
  [this k make-linked-map assoc-fn dissoc-fn]
  (let [head     (-get-head this)
        delegate (-get-delegate this)]
    (if-let [entry (get delegate k)]
      (if (= 1 (count delegate))
        (make-linked-map head (dissoc-fn delegate k))
        (let [rk   (.-r ^Node entry)
              lk   (.-l ^Node entry)
              head (if (= k head) rk head)]
          (make-linked-map head
                           (-> delegate
                               (dissoc-fn k)
                               (update* assoc-fn rk update-node-left lk)
                               (update* assoc-fn lk update-node-right rk)))))
      this)))

(defn- cons*
  [assoc-fn this o]
  (cond
    (map-entry? o)
    (assoc-fn this (key o) (val o))

    (vector? o)
    (if (= 2 (count o))
      (assoc-fn this (nth o 0) (nth o 1))
      (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                  "vector arg to map conj must be a pair")))

    ;; Maybe add support for transient to speed up multiple assoc?
    ;; (on my own benchmark it has not much difference)
    :else
    (reduce (partial cons* assoc-fn) this o)))

(defn- assoc-after*
  [this key k v make-linked-map assoc-fn dissoc-fn]
  (let [delegate (-get-delegate this)]
    (if (empty? delegate)
      (make-linked-map k (assoc-fn delegate k (make-node k v k k)))
      (if (contains? delegate key)
        (if (contains? delegate k)
          (-> (dissoc* this k make-linked-map assoc-fn dissoc-fn)
              (assoc-after* key k v make-linked-map assoc-fn dissoc-fn))

          (let [target-node (get delegate key)
                head        (-get-head this)
                trk         (.-r ^Node target-node)
                income-node (make-node k v key trk)
                target-node (-> target-node
                                (update-node-right k)
                                (cond-> (and (= trk head)
                                             (= key head))
                                  (update-node-left k)))
                delegate    (-> delegate
                                (update* assoc-fn trk update-node-left k)
                                (assoc-fn key target-node)
                                (assoc-fn k income-node))]
            (make-linked-map head delegate)))

        (if (nil? key)
          (let [this        (if (contains? delegate k)
                              (dissoc* this k make-linked-map assoc-fn dissoc-fn)
                              this)

                delegate    (-get-delegate this)
                head        (-get-head this)

                target-node (get delegate head)
                tlk         (.-l ^Node target-node)
                trk         (.-r ^Node target-node)
                income-node (make-node k v tlk head)
                target-node (-> target-node
                                (update-node-left k)
                                (cond-> (= trk head)
                                  (update-node-right k)))
                delegate    (-> delegate
                                (update* assoc-fn trk update-node-right k)
                                (assoc-fn head target-node)
                                (assoc-fn k income-node))]
            (make-linked-map k delegate))
          this)))))

(defn- assoc-before*
  [this key k v make-linked-map assoc-fn dissoc-fn]
  (let [head     (-get-head this)
        delegate (-get-delegate this)]

    (if (empty? delegate)
      (make-linked-map k (assoc-fn delegate k (make-node k v k k)))
      (if (contains? delegate key)
        (if (contains? delegate k)
          (-> (dissoc* this k make-linked-map assoc-fn dissoc-fn)
              (assoc-before* key k v make-linked-map assoc-fn dissoc-fn))
          (let [target-node (get delegate key)
                tlk         (.-l ^Node target-node)
                income-node (make-node k v tlk key)
                delegate    (-> delegate
                                (update* assoc-fn tlk update-node-right k)
                                (assoc-fn key (update-node-left target-node k))
                                (assoc-fn k income-node))
                head        (if (= key head) k head)]
            (make-linked-map head delegate)))
        (if (nil? key)
          (assoc* this k v make-linked-map assoc-fn)
          this)))))

(defn- rename-key*
  [^LinkedMap this key k make-linked-map assoc-fn dissoc-fn]
  (let [delegate (.-delegate this)]
    (if (or (empty? delegate)
            (= key k))
      this
      (if-let [target-node (some-> (get delegate key)
                                   (update-node-key k))]
        (let [tlk      (.-l ^Node target-node)
              trk      (.-r ^Node target-node)
              delegate (-> delegate
                           (dissoc-fn key)
                           (assoc-fn k target-node)
                           (update* assoc-fn tlk update-node-right k)
                           (update* assoc-fn trk update-node-left k))
              head     (.-head this)
              head     (if (= key head)
                         k
                         head)]
          (make-linked-map head delegate))
        this))))

;;;; reduce

(defn- reduce*
  [delegate f fkey lkey result]
  (loop [current fkey
         result  result]
    (let [entry  (get delegate current)
          result (f result entry)]
      (cond
        (reduced? result)
        @result

        (= current lkey)
        result

        :else
        (recur (.-r ^Node entry) result)))))

(defn- kvreduce*
  [delegate head f init]
  (if (pos? (count delegate))
    (let [head-node (get delegate head)
          tail      (.-l ^Node head-node)]
      (loop [state  (f init (.-k ^Node head-node) (.-v ^Node head-node))
             next-k (.-r ^Node head-node)]

        (cond
          (reduced? state)
          @state

          (= next-k tail)
          (let [node  (get delegate next-k)
                state (f state (.-k ^Node node) (.-v ^Node node))]
            (if (reduced? state)
              @state
              state))

          :else
          (let [node (get delegate next-k)]
            (recur (f state (.-k ^Node node) (.-v ^Node node))
                   (.-r ^Node node))))))
    init))

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
    (when (pos? (count delegate))
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
  (LinkedMap. nil {} (hash {})))

(defn ->map
  [o]
  (into (LinkedMap. nil {} (hash {})) o))

(defn map
  ([] empty-map)
  ([& kvpairs] (apply assoc empty-map kvpairs)))

(defn map?
  [o]
  (instance? LinkedMap o))

(defn assoc-after
  ([m key k v]
   (-assoc-after m key k v))
  ([m key k v & kv]
   (loop [m   (-assoc-after m key k v)
          kv  (seq kv)
          key k
          k   nil]
     (if-let [r (first kv)]
       (if (nil? k)
         (recur m (rest kv) key r)
         (recur (-assoc-after m key k r) (rest kv) k nil))
       (if (nil? k)
         m
         (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                     "assoc-after expects even number of arguments")))))))

(defn assoc-before
  ([m key k v]
   (-assoc-before m key k v))
  ([m key k v & kv]
   (loop [m  (-assoc-before m key k v)
          kv (seq kv)
          k  nil]
     (if-let [r (first kv)]
       (if (nil? k)
         (recur m (rest kv) r)
         (recur (-assoc-before m key k r) (rest kv) nil))
       (if (nil? k)
         m
         (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                     "assoc-before expects even number of arguments")))))))

(defn rename-key
  ([m k k']
   (-rename-key m k k'))
  ([m k k' & kv]
   (loop [m  (-rename-key m k k')
          kv (seq kv)
          k  nil]
     (if-let [r (first kv)]
       (if (nil? k)
         (recur m (rest kv) r)
         (recur (-rename-key m k r) (rest kv) nil))
       (if (nil? k)
         m
         (throw (new #?(:clj IllegalArgumentException :cljs js/Error)
                     "assoc-before expects even number of arguments")))))))

;; (defn assoc1
;;   ([m k v]
;;    (clojure.lang.RT/assoc m k v))
;;   ([m k v & kv]
;;    (loop [m  (clojure.lang.RT/assoc m k v)
;;           kv (seq kv)
;;           k  nil]
;;      (if-let [r (first kv)]
;;        (if (nil? k)
;;          (recur m (rest kv) r)
;;          (recur (clojure.lang.RT/assoc m k r) (rest kv) nil))
;;        (if (nil? k)
;;          m
;;          (throw (IllegalArgumentException.
;;                  "assoc-after expects even number of arguments")))))))

#?(:cljs (reader/register-tag-parser! 'bestellt/map ->map))
