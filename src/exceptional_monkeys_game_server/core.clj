(ns exceptional-monkeys-game-server.core
  (:require [clj-uuid :as uuid]
            [cheshire.core :as json]
            [org.httpkit.server :as http-server])
  (:use [compojure.core :only [GET]]))

(def exceptionTypes ["IOException", "DivideByZeroException", "NullPointerException", "IndexOutOfBoundsException"])
(def items (atom {}))
(def players (atom {}))
(def playerWidth 0.1)
(def playerHeight 0.13)
(def itemWidth 0.1)
(def itemHeight 0.05)

(defn send-msg [connection msg]
  (http-server/send! connection (json/generate-string msg {:pretty true})))

(defn broadcast-msg [msg]
  (doseq [connection (keys @players)]
    (send-msg connection msg)))

(defn remove-collected-item [item]
  (swap! items dissoc (key item))
  (broadcast-msg (assoc (val item) :show false)))

(defn exceptions-generator []
  (future (loop []
            (let [new-val {:exception?    true
                           :show          true
                           :exceptionType (rand-nth exceptionTypes)
                           :x             (rand)
                           :y             (rand)}
                  id (str (uuid/v1))]
              (swap! items assoc id new-val)
              (broadcast-msg new-val))
            (Thread/sleep 5000)
            (recur))))

(defn collision? [playerX playerY itemX itemY]
  (and (< playerX (+ itemX itemWidth))
       (> (+ playerX playerWidth) itemX)
       (< playerY (+ itemY itemHeight))
       (> (+ playerY playerHeight) itemY)))

(defn get-collected-item [player]
  (let [pred (fn [[_ v]]
               (and (= (:exceptionType v) (:exceptionType player))
                    (collision? (:x player) (:y player) (:x v) (:y v))))]
    (first (filter pred @items))))

(defn update-player-in-map [connection player]
  (swap! players assoc connection player)
  player)

(defn collect-item [player connection]
  (let [collected (get-collected-item player)]
    (if (some? collected)
      (do
        (remove-collected-item collected)
        (update-player-in-map connection (assoc player :score (+ 1 (:score player)))))
      player)))

(defn move-player [player stepX stepY connection]
  (let [x (+ stepX (:x player))
        y (+ stepY (:y player))]
    (if (or (< y 0) (< x 0) (> y 1) (> x 1))
      (assoc player :collision true)
      (update-player-in-map connection (assoc player :x x :y y)))))

(defn get-new-player [ ]
  {:player?       true
   :id            (str (uuid/v1))
   :x             (rand)
   :y             (rand)
   :score         0
   :show          true
   :exceptionType (rand-nth exceptionTypes)
   :collision     false})

(defn remove-player [connection]
  (let [player (@players connection)]
    (swap! players dissoc connection)
    (broadcast-msg (assoc player :show false))))

(defn add-new-player [player connection]
  ;; send self to client
  (send-msg connection (assoc player :self? true))
  ;; send all existing players to client
  (doseq [existing-player (vals @players)] (send-msg connection existing-player))
  ;; send to client all existing exceptions
  (doseq [existing (vals @items)] (send-msg connection existing))
  ;; send new player to all existing players
  (broadcast-msg player)
  ;; add player to list.
  (update-player-in-map connection player))

(defn move-and-collect [connection stepX stepY]
  (-> (move-player (@players connection) stepX stepY connection)
      (collect-item connection)
      (broadcast-msg)))

(defn process-message [connection message]
  (let [data (json/parse-string message keyword)]
    (if (contains? @players connection)
      (move-and-collect connection (:stepX data) (:stepY data))
      (add-new-player
        (get-new-player) connection))))

(defn ws-handler [request]
  (http-server/with-channel request channel
                            (http-server/on-close channel (fn [status]
                                                            (println "connection closed:" status)
                                                            (remove-player channel)))
                            (http-server/on-receive channel (fn [data]
                                                              (process-message channel data)))))

(def websocket-routes
  (GET "/" [] ws-handler))

(defn -main [& {:as args}]
  (println "Starting exceptional monkeys server... ")
  (exceptions-generator)
  (http-server/run-server websocket-routes {:port 8080}))
