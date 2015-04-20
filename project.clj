(defproject clj-gameoflife "0.1.0-SNAPSHOT"
  :description "Conway's Game of Life"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot clj-gameoflife.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
