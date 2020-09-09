(defproject weaving "0.2.2"
  :description "Combinators to weave Clojure[Script] functions together"
  :url "https://github.com/TristeFigure/weaving"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure        "1.10.0"   :scope "provided"]
                 [org.clojure/clojurescript  "1.10.520" :scope "provided"]]
  :plugins [[lein-codox     "0.10.3"]
            [lein-cljsbuild "1.1.7"]
            [lein-doo       "0.1.11"]]
  :prep-tasks ["compile" ["cljsbuild" "once"]]
  :cljsbuild {:builds {}}

  :profiles
  {:common {:cljsbuild
            {:builds
             {:main {:source-paths ["src"]
                     :compiler     {:output-to     "target/js/main/weaving.js"
                                    :output-dir    "target/js/main/"
                                    :optimizations :none}}}}}
   :dev [:common
         {:dependencies
          [[org.clojars.tristefigure/shuriken "0.14.40" :exclusions [weaving]]
           [doo "0.1.11"]
           [codox-theme-rdash "0.1.2"]]}]
   :test [:dev
          {:cljsbuild
           {:builds
            {:main {:source-paths ["test"]
                    :compiler     {:target :nodejs
                                   :main   weaving.doo-test
                                   :output-to  "target/js/test/weaving.js"
                                   :output-dir "target/js/test/"}}}}}]
   :release [:common
             {:cljsbuild
              {:builds
               {:main {:jar      true
                       :compiler {:optimizations :advanced}}}}}]}

  :doo {:build "main"
        :alias {:default [:node]}}
  :aliases {"test"    ["with-profile" "test" ["do" ["test"] ["doo" "once"]]]
            "deploy"  ["with-profile" "release" "deploy"]
            "release" ["with-profile" "release" "release"]}
  :codox {:source-uri "https://github.com/TristeFigure/weaving/blob/{version}/{filepath}#L{line}"
          :metadata   {:doc/format :markdown}
          :themes     [:rdash]})
