(defproject playground "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main playground.core
  :aot :all
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[refactor-nrepl "2.0.0"]
                             [cider/cider-nrepl "0.10.2"]]}})



  





