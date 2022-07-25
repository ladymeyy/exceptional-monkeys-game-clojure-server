
(defproject exceptional-monkeys-game-server "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [http-kit "2.5.0"]
                 [compojure "1.6.1"]
                 [danlentz/clj-uuid "0.1.7"]
                 [cheshire "5.6.3"]]
  :repl-options {:init-ns exceptional-monkeys-game-server.core}
  :source-paths ["src"]
  :main exceptional-monkeys-game-server.core)
