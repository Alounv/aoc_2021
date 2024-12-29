(defproject aoc_2021 "0.1.0-SNAPSHOT"
  :description "Advent of Code 2021 Solutions"
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "1.1.1"]]}}
  :test-paths ["src"]
  :repl-options {:init-ns aoc-2021.core})
