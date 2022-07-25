(ns exceptional-monkeys-game-server.core
  (:require [clj-uuid :as uuid]
            [cheshire.core :as json]
            [org.httpkit.server :as http-server])
  (:use [compojure.core :only [GET]]))

(def exceptionTypes ["IOException", "DivideByZeroException", "NullPointerException", "IndexOutOfBoundsException"])
(def collectables (atom {}))
(def players (atom {}))

(defn send-msg [connection msg]
  (http-server/send! connection (json/generate-string msg {:pretty true})))

(defn broadcast-msg [msg]
  (doseq [connection (keys @players)]
    (send-msg connection msg)))

(defn update-collectables-game-state [collected]
  (swap! collectables dissoc (key collected))
  (broadcast-msg (assoc (val collected) :show false)))

(defn generate-rand-exception []
  (future (loop []
            (let [max 800
                  min 60
                  new-val {:exception?    true
                           :show          true
                           :exceptionType (rand-nth exceptionTypes)
                           :x             (+ min (rand-int (- max min)))
                           :y             (+ min (rand-int (- max min)))}
                  id (str (uuid/v1))]
              (swap! collectables assoc id new-val)
              (broadcast-msg new-val))
            (Thread/sleep 5000)
            (recur))))

(defn is-player-overlap? [playerX playerY exX exY]
  (not (and (or (> playerX (+ 130 exX)) (> exX (+ 100 playerX)))
            (or (< (+ playerY 129) exY) (< (+ exY 200) playerY)))))

(defn collect [player connection]
  (let [pred (fn [[_ v]]
               (and (= (:exceptionType v) (:exceptionType player))
                    ;; calculate if player collected an item.
                    (is-player-overlap? (:x player) (:y player) (:x v) (:y v))))
        collected (first (filter pred @collectables))]
    (if (some? collected)
      (do
        ;; if item was collected remove it from items map
        (update-collectables-game-state collected)
        ;; update player new score in players map
        (swap! players assoc connection
               (assoc player :score (+ 1 (:score player))))
        ;; return updated player
        (assoc player :score (+ 1 (:score player))))
      ;; else return input player
      player)))

(defn move-player [player moveX moveY connection]
  (let [x (+ moveX (:x player))
        y (+ moveY (:y player))]
    (if (or (< y 0) (< x 0) (>= x (:windowW player)) (>= y (:windowH player)))
      (assoc player :collision true)
      (do
        ;; update player in players map
        (swap! players assoc connection (assoc player :x x :y y))
        ;; return updated player
        (assoc player :x x :y y)))))

(defn generate-new-player [windowH windowW]
  {:player?       true
   :id            (str (uuid/v1))
   :x             (rand-int 600)
   :y             (rand-int 300)
   :score         0
   :show          true
   :exceptionType (rand-nth exceptionTypes)
   :collision     false
   :windowH windowH
   :windowW windowW})

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
  (doseq [existing (vals @collectables)] (send-msg connection existing))
  ;; send new player to all existing players
  (broadcast-msg player)
  ;; add player to list.
  (swap! players assoc connection player))

(defn update-player-state [connection newX newY]
  (-> (move-player (@players connection)
                   (Integer/parseInt newX)
                   (Integer/parseInt newY)
                   connection)
      (collect connection)
      (broadcast-msg)))

(defn process-message [connection message]
  (let [data (json/parse-string message keyword)]
    (if (contains? @players connection)
      (update-player-state connection (:x data) (:y data))
      (add-new-player
        (generate-new-player (:height data) (:width data)) connection))))


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
  (generate-rand-exception)
  (http-server/run-server websocket-routes {:port 8080}))
