(defproject snake "0.1.0-SNAPSHOT"
  :description "A snake game made in Clojure"
  :url "www.stenh.com/snake/index.html"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0-RC1"]
                 [quil "2.2.6"]
                 [org.clojure/clojurescript "0.0-3308"]]
  :plugins [[lein-cljsbuild "1.0.6"]]
  :hooks [leiningen.cljsbuild]
  :cljsbuild
  {:builds [{:source-paths ["src"]
              :compiler
               {:output-to "js/main.js"
                :output-dir "out"
                :main "snake.core"
                :optimizations :none
                :pretty-print true}}]})
