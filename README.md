# Bestellt

Map and Set structures that rememeber the insertion order of its
elements, even after multiple assoc and dissoc. For Clojure and
ClojureScript.

Fork of [linked](https://github.com/frankiesardo/linked)



## Install

TBD


## Getting Started

### Ordered Map

```clj
(require '[bestellt.map :as bm])

(bm/map :b 2 :a 1 :d 4)
;=> #bestellt/map [[:b 2] [:a 1] [:d 4]]

(assoc (bm/map :b 2 :a 1 :d 4) :c 3)
;=> #bestellt/map [[:b 2] [:a 1] [:d 4] [:c 3]]

(into (bm/map) [[:c 3] [:a 1] [:d 4]])
;=> #bestellt/map [[:c 3] [:a 1] [:d 4]]

(dissoc (bm/map :c 3 :a 1 :d 4) :a)
;=> #bestellt/map [[:c 3] [:d 4]]
```

### Ordered Set

```clj
(require '[bestellt.set :as bs])

(bs/set 4 3 1 8 2)
;=> #bestellt/set [4 3 1 8 2]

(conj (bs/set 9 10) 1 2 3)
;=> #bestellt/set [9 10 1 2 3]

(into (bs/set) [7 6 1 5 6])
;=> #bestellt/set [7 6 1 5]

(disj (bs/set 8 1 7 2 6) 7)
;=> #bestellt/set [8 1 2 6]
```

## Performance

In the same way as the [linked][1], the bestellt implementation of Map
and Set are based on the double-linked structures. Behind the scenes
it uses the default hash-map for store the nodes.

These data structures wrap a normal `hash-map` but instead of feeding
it a normal `[key value]` pair their remeber a `[key value left-key
right-key]` record.

When an item is added, removed or updated on the data structure, it is
sufficient to update the value or the left and right references.  This
implementation yields the same Big O time and space complexity of a
standard `hash-map` (altought effective performance will be slower by
a constant factor).

## Comaprison with [linked][1]

The main idea behind the fork is add performance and efficiency
enhacements and allow nativelly relative operations such that
`assoc-after`, `assoc-before` and `rename-key`.

Bestellt also implements transients (only for maps for now, transient
support for sets is in development)


### Benchmarks

Here we have some benchmark comparing the same operation between
[linked][1], bestellt and native unordered clojure hash-maps.

**reduce-kv**

```
user=> (bench-reduce-kv)
=> linked
Evaluation count : 1018200 in 6 samples of 169700 calls.
      Execution time sample mean : 616.361476 ns
             Execution time mean : 614.565151 ns
Execution time sample std-deviation : 19.356864 ns
    Execution time std-deviation : 20.213395 ns
   Execution time lower quantile : 596.419776 ns ( 2.5%)
   Execution time upper quantile : 644.494228 ns (97.5%)
                   Overhead used : 1.599203 ns
=> bestellt
Evaluation count : 3928638 in 6 samples of 654773 calls.
      Execution time sample mean : 152.434017 ns
             Execution time mean : 152.489722 ns
Execution time sample std-deviation : 2.013334 ns
    Execution time std-deviation : 2.112402 ns
   Execution time lower quantile : 150.527125 ns ( 2.5%)
   Execution time upper quantile : 155.229062 ns (97.5%)
                   Overhead used : 1.599203 ns
=> native
Evaluation count : 11940642 in 6 samples of 1990107 calls.
      Execution time sample mean : 50.189976 ns
             Execution time mean : 50.177860 ns
Execution time sample std-deviation : 1.277982 ns
    Execution time std-deviation : 1.369512 ns
   Execution time lower quantile : 48.617572 ns ( 2.5%)
   Execution time upper quantile : 51.530898 ns (97.5%)
                   Overhead used : 1.599203 ns
```

**assoc to big map**

```
user=> (bench-assoc-big)
=> linked
Evaluation count : 715782 in 6 samples of 119297 calls.
      Execution time sample mean : 899.928561 ns
             Execution time mean : 899.928561 ns
Execution time sample std-deviation : 57.488648 ns
    Execution time std-deviation : 61.111008 ns
   Execution time lower quantile : 839.258271 ns ( 2.5%)
   Execution time upper quantile : 978.424569 ns (97.5%)
                   Overhead used : 1.599203 ns
=> bestellt
Evaluation count : 1330152 in 6 samples of 221692 calls.
      Execution time sample mean : 459.464155 ns
             Execution time mean : 459.927646 ns
Execution time sample std-deviation : 13.810955 ns
    Execution time std-deviation : 13.848051 ns
   Execution time lower quantile : 449.969201 ns ( 2.5%)
   Execution time upper quantile : 482.669684 ns (97.5%)
                   Overhead used : 1.599203 ns
=> native
Evaluation count : 4982148 in 6 samples of 830358 calls.
      Execution time sample mean : 118.905647 ns
             Execution time mean : 118.912893 ns
Execution time sample std-deviation : 5.311423 ns
    Execution time std-deviation : 5.311423 ns
   Execution time lower quantile : 115.306005 ns ( 2.5%)
   Execution time upper quantile : 128.030137 ns (97.5%)
                   Overhead used : 1.599203 ns
```


**equality of big maps**

```
user=> (bench-equality-big)
=> linked
Evaluation count : 779634 in 6 samples of 129939 calls.
      Execution time sample mean : 794.956102 ns
             Execution time mean : 796.831286 ns
Execution time sample std-deviation : 34.606645 ns
    Execution time std-deviation : 35.235382 ns
   Execution time lower quantile : 770.431456 ns ( 2.5%)
   Execution time upper quantile : 848.290045 ns (97.5%)
                   Overhead used : 1.599203 ns
=> bestellt
Evaluation count : 1060530 in 6 samples of 176755 calls.
      Execution time sample mean : 574.138089 ns
             Execution time mean : 573.890760 ns
Execution time sample std-deviation : 10.081862 ns
    Execution time std-deviation : 10.504107 ns
   Execution time lower quantile : 564.841594 ns ( 2.5%)
   Execution time upper quantile : 585.979713 ns (97.5%)
                   Overhead used : 1.599203 ns
=> native
Evaluation count : 2937864 in 6 samples of 489644 calls.
      Execution time sample mean : 207.004135 ns
             Execution time mean : 207.004135 ns
Execution time sample std-deviation : 3.334755 ns
    Execution time std-deviation : 3.624218 ns
   Execution time lower quantile : 202.983709 ns ( 2.5%)
   Execution time upper quantile : 210.572522 ns (97.5%)
                   Overhead used : 1.599203 ns

```


**assoc-before**

The `assoc-before` is not nativelly supported so it is emulated with
basic map and seq manipulation. I use a sample implementation using
`reduce-kv`.

The implementation used for this case is:

```clojure
(defn assoc-before
  [m before-k k v]
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
```

The result:

```
user=> (bench-assoc-before)
=> linked
Evaluation count : 275094 in 6 samples of 45849 calls.
      Execution time sample mean : 2.173840 µs
             Execution time mean : 2.176430 µs
Execution time sample std-deviation : 52.785092 ns
    Execution time std-deviation : 54.317345 ns
   Execution time lower quantile : 2.134116 µs ( 2.5%)
   Execution time upper quantile : 2.256374 µs (97.5%)
                   Overhead used : 1.599203 ns
=> bestellt
Evaluation count : 2540784 in 6 samples of 423464 calls.
      Execution time sample mean : 242.253658 ns
             Execution time mean : 242.253658 ns
Execution time sample std-deviation : 14.728219 ns
    Execution time std-deviation : 14.843461 ns
   Execution time lower quantile : 231.452319 ns ( 2.5%)
   Execution time upper quantile : 266.985546 ns (97.5%)
                   Overhead used : 1.599203 ns
```

## License

Copyright © 2024-Now Andrey Antukh
Copyright © 2014 Frankie Sardo

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.


[1](https://github.com/frankiesardo/linked)
