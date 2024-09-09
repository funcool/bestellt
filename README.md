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
(require '[bestellt.core :as linked])

(linked/map :b 2 :a 1 :d 4)
;=> #bestellt/map [[:b 2] [:a 1] [:d 4]]

(assoc (linked/map :b 2 :a 1 :d 4) :c 3)
;=> #bestellt/map [[:b 2] [:a 1] [:d 4] [:c 3]]

(into (linked/map) [[:c 3] [:a 1] [:d 4]])
;=> #bestellt/map [[:c 3] [:a 1] [:d 4]]

(dissoc (linked/map :c 3 :a 1 :d 4) :a)
;=> #bestellt/map [[:c 3] [:d 4]]
```

### Ordered Set

```clj
(require '[bestellt.core :as linked])

(linked/set 4 3 1 8 2)
;=> #bestellt/set [4 3 1 8 2]

(conj (linked/set 9 10) 1 2 3)
;=> #bestellt/set [9 10 1 2 3]

(into (linked/set) [7 6 1 5 6])
;=> #bestellt/set [7 6 1 5]

(disj (linked/set 8 1 7 2 6) 7)
;=> #bestellt/set [8 1 2 6]
```

## Performance

These data structures wrap a normal `hash-map` but instead of feeding
it a normal `[key value]` pair their remeber a `[key value left-key
right-key]` record. When an item is removed from the data structure it
is sufficient to update the left and right node to reference each
others keys while removing the chosen node. This implementation yields
the same Big O time and space complexity of a standard `hash-map`
(altought effective performance will be slower by a constant factor).

## Comparison with [ordered](https://github.com/amalloy/ordered)

- Ordered need an explicit call to `compact` for properly freeing
  allocated memory for old objects. Bestellt does not keep pointers to
  old elements.
- Ordered has transient support for faster allocation of a large
  number of items; Bestellt does not support transients at this
  moment.

## Comaprison with [linked](https://github.com/frankiesardo/linked)

TBD


## License

Copyright © 2024-Now Andrey Antukh
Copyright © 2014 Frankie Sardo

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
