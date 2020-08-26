(defproject exceptional-monkeys-game-server "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.immutant/immutant "2.1.10"]
                 [compojure "1.6.1"]
                 [ring/ring-core "1.7.1"]
                 [environ "1.0.0"]
                 [danlentz/clj-uuid "0.1.7"]
                 [cheshire "5.6.3"]]
  :main exceptional-monkeys-game-server.core
  :repl-options {:init-ns exceptional-monkeys-game-server.core})
