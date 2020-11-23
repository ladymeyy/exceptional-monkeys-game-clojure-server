(ns exceptional-monkeys-game-server.core
  (:require
    [clj-uuid :as uuid]
    [cheshire.core :as json])
  (:use [compojure.route :only [files not-found]]
        [compojure.core :only [defroutes GET POST DELETE ANY context]]
        org.httpkit.server)
  (:gen-class))

;UPDATE PLAYER MAP:
;player has exp.seq. instead of exp.type

;ADD FUNCTION:
;add function that generates a random exp.seq. of len 10.


;UPDATE GEN-RAND-EX FUNCTION
;make exp. disappear faster.

;
;UPDATE COLLISION FUNCTION:
;- upon collision of player with an exception - check if collided exp matched the first exp. in the exp sequence of the player.
;	-  if it is not - than add it to the sequence of the player.
;	- if it is - remove from player seq.
; if seq of player is empty - send - WIN to all clients --- than clients should close connections.

;TODO
;add timestamp - var or atom ? being set with first client which connects.
;server sends timestamp to every new player

(def exceptionTypes ["IOException", "DivideByZeroException", "NullPointerException",  "IndexOutOfBoundsException", "InterruptedException", "RuntimeException"])
(def collectables (atom {}))
(def players (atom {}))

(defn send-msg [con msg] (send! con (json/generate-string msg {:pretty true})))

(defn broadcast-msg [msg] (doseq [con (keys @players)] (send-msg con msg)))

(defn update-collectables-game-state [collected]
  (swap! collectables dissoc (key collected))
  (broadcast-msg (assoc (val collected) :show false)))

(defn show-rand-ex []
  (future (loop []
            (let [max 800
                  min 60
                  new-val {:exception?    true
                           :show          true
                           :exceptionType (rand-nth exceptionTypes)
                           :x             (+ min (rand-int (- max min)))
                           :y             (+ min (rand-int (- max min)))}]
              (swap! collectables assoc (str (uuid/v1)) new-val)
              (broadcast-msg new-val))
            (Thread/sleep 1000)
            (recur))))

(defn overlap? [playerX playerY exX exY]
  (cond
    (or (> playerX (+ 130 exX)) (> exX (+ 100 playerX))) false
    (or (< (+ playerY 129) exY) (< (+ exY 200) playerY)) false
    :else true))

(defn collect [player connection]
  (let [pred (fn [[k v]] (overlap? (:x player) (:y player) (:x v) (:y v))) ;check if player collided with an item.
        collided (first (filter pred @collectables))
        res (= (:exceptionType (val collided)) (first (:exceptionType player)))]
    (if res
      (do
        (update-collectables-game-state collided)  ;if item was collected remove it from items map,
        (swap! players assoc connection (assoc player :exceptionType (rest (:exceptionType player))) ) ;update player new score in players map
        (assoc player :exceptionType (rest (:exceptionType player))) )        ; return updated player
        (assoc player :exceptionType  (conj (:exceptionType player) (:exceptionType (val collided)) ) ))))                                             ; else return input player

(defn move-player [player moveX moveY connection]
  (let [x (+ moveX (:x player))
        y (+ moveY (:y player))]
    (if (or (< y 0) (< x 0) (>= x (:windowW player)) (>= y (:windowH player)))
      (assoc player :collision true)
      (do
        (swap! players assoc connection (assoc player :x x :y y)) ;update player in players map
        (assoc player :x x :y y)))))                        ; return updated player

(defn is-winner [player]
  (if (empty? (:exceptionType player))
    (do
      (broadcast-msg (assoc {} :winner (:id player)))
      true)
    false))

(defn new-player [windowH windowW]
  {:player?       true
   :id            (str (uuid/v1))
   :x             (rand-int 600)
   :y             (rand-int 300)
   :score         0
   :show          true
   :exceptionType (take 5 (repeatedly #(rand-nth exceptionTypes))) ;TODO rename this to expSeq
   :color         [(rand-int 256) (rand-int 256) (rand-int 256)]
   :collision     false
   :windowH windowH
   :windowW windowW})

(defn remove-player [connection]
 (let [player (@players connection)]
   (swap! players dissoc connection)
   (broadcast-msg (assoc player :show false))))

(defn add-new-player [player connection]
  (send-msg connection (assoc player :self? true));send self to client
  (doseq [existing-player (vals @players)] (send-msg connection existing-player));send all existing players to client
  (doseq [existing (vals @collectables)] (send-msg connection existing)) ;send to client all existing exceptions
  (broadcast-msg player)  ;send new player to all existing players
  (swap! players assoc connection player)) ;add player to list.

(defn update-player-state [connection newX newY]
  (-> (move-player (@players connection) (Integer/parseInt newX) (Integer/parseInt newY) connection)
      (collect connection)
      (broadcast-msg))
  ; TODO  add here : (fn [player] (when-not (is-winner player) (broadcast-msg player) ) )
  )

(defn process-message [connection message]
  (let [json-data (json/parse-string message keyword)]
    (if (and (:height json-data) (:width json-data))
      (add-new-player (new-player (:height json-data) (:width json-data)) connection)
      (update-player-state connection (:x json-data) (:y json-data)))))

(defn ws-handler [request]
  (with-channel request channel
                (on-close channel (fn [status] (println "connection closed:" status) (remove-player channel)))
                (on-receive channel (fn [data] (process-message channel data)) )))

(def websocket-routes
  (GET "/" [] ws-handler) )

(defn -main [& {:as args}]
  (show-rand-ex)
  (run-server websocket-routes {:port 8080}))