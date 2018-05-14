# weaving

*Weaving is to lambdas what threading is to s-expressions.*

This library's purpose is to provide a set of simple function combinators to ease out the day-to-day wrangling of Clojure code.

Just like **threading macros** in Clojure end with `->` or `->>`, **weaving functions** end with `|`.

## Usage

```clojure
(def f
  (->| (when| number? (| * 2))
       (if| (or| string? symbol?)
         (|| str "The string ")
         (->| str
              (tap| (|| println "Warning: not a string:"))))
        vector
        (apply| (| clojure.string/replace "0" "X"))))

(f 100)
;; Warning: not a string: 200
;; => "2XX"

(f "slime")
;; => "The string slime"
```

## List of combinators

- conditionals: `if|`, `when|`, `or|`, `and|` & `not|`.
- partialization: `|` & `||`. On the model of `->` and `->>`,
  - `(| str "ABC")` produces strings like `"_ABC"` while
  - `(|| str "ABC")` produces strings like `"ABC_"`.
- function arguments weaving: `•|`,
    - `((•| + identity (constantly 1))  2)` yields `3`.
- other control flow: `tap|` [inspired by ruby](https://apidock.com/ruby/Object/tap).
- `<-|` : works like `constantly`.
- `*|`: works like `juxt`.
- `?|`: `(?| 2)` is equivalent to `#(= % 2)`.
- `->|`: works like `comp` but in a reverse, sane order (the same as `->`).
- `apply|`.

## License

Copyright © 2018 TristeFigure

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
