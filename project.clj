(defproject weaving "0.1.4"
  :description "Combinators to weave Clojure functions together"
  :url "https://github.com/TristeFigure/weaving"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [arity "0.2.0"]]
  :profiles {:test {:dependencies
                    [[org.clojars.tristefigure/shuriken "0.14.28"]]}
             :dev  {:dependencies
                    [[org.clojars.tristefigure/shuriken "0.14.28"]
                     [codox-theme-rdash "0.1.2"]]}}
  :plugins [[lein-codox "0.10.3"]]
  :codox {:source-uri "https://github.com/TristeFigure/weaving/" \
                      "blob/{version}/{filepath}#L{line}"
          :metadata {:doc/format :markdown}
          :themes [:rdash]})
