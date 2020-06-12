(defproject wcl-query-builder "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.json "1.0.0"]
                 [rm-hull/infix "0.3.3"]]
  :plugins [[lein-cljfmt "0.6.7"]]
  :main ^:skip-aot wcl-query-builder.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
