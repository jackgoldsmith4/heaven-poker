(defproject heaven-poker "0.1.0-SNAPSHOT"
  :description "Online poker with friends"
  :url ""
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-ring "0.12.5"]]
  :ring {:handler heaven-poker.core/home
         :auto-reload? true
         :auto-refresh false}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [ring/ring "1.8.1"]
                 [hiccup "2.0.0-alpha2"]
                 [compojure "1.6.1"]]
  :repl-options {:init-ns heaven-poker.core}
  :main heaven-poker.core/main)
