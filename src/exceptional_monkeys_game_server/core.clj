(ns exceptional-monkeys-game-server.core
  (:require
    [clj-uuid :as uuid]
    [cheshire.core :as json])
  (:use [compojure.route :only [files not-found]]
        [compojure.core :only [defroutes GET POST DELETE ANY context]]
        org.httpkit.server)
  (:gen-class))

(def exceptionTypes ["IOException", "DivideByZeroException", "NullPointerException",  "IndexOutOfBoundsException", "InterruptedException", "RuntimeException"])
(def collectables (atom {}))
(def players (atom {}))

(defn broadcast-msg [connections msg]
  (doseq [[con p] connections] (send! con (json/generate-string msg {:pretty true}))))

(defn update-collectables-game-state [collected]
  (swap! collectables dissoc (key collected))
  (broadcast-msg @players (assoc (val collected) :show false)))

(defn update-game-state
  ([channel]
   "Remove player"
   (let [player (@players channel)]
     (-> (swap! players dissoc channel)
         (broadcast-msg (assoc player :show false)))))

  ([plyr channel]
   "Add new player "
   (doseq [p (conj (vals @players) (assoc plyr :self? true))] (broadcast-msg (assoc {} channel true) p)) ;send to self existing players
   (update-game-state plyr channel assoc))

  ([player channel edit-func]                               ;remove edit func
   "Edit exsiting player values "
   (-> (swap! players edit-func channel (assoc player :collision false))
       (broadcast-msg player))))

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
              (broadcast-msg @players new-val))
            (Thread/sleep 5000)
            (recur))))

(defn overlap? [playerX playerY exX exY]
  (cond
    (or (> playerX (+ 130 exX)) (> exX (+ 100 playerX))) false
    (or (< (+ playerY 129) exY) (< (+ exY 200) playerY)) false
    :else true))

(defn collect [player]
  (let [pred (fn [[k v]] (and (= (:exceptionType v) (:exceptionType player))
                              (overlap? (:x player) (:y player) (:x v) (:y v))))
        collected (first (filter pred @collectables))]
    collected))

(defn move-player [player moveX moveY]
  (let [x (+ moveX (:x player))
        y (+ moveY (:y player))]
    (if (or (< y 0) (< x 0) (>= x (:windowW player)) (>= y (:windowH player)))
      (assoc player :collision true)
      (assoc player :x x :y y))))

(defn new-player [windowH windowW]
  {:player?       true
   :id            (str (uuid/v1))
   :x             (rand-int 600)
   :y             (rand-int 300)
   :score         0
   :show          true
   :exceptionType (rand-nth exceptionTypes)
   :color         [(rand-int 256) (rand-int 256) (rand-int 256)]
   :collision     false
   :windowH windowH
   :windowW windowW})

(defn handle-msg! [channel msg]
  (let [{:keys [x y]} (json/parse-string msg keyword)
        player (move-player (@players channel) (Integer/parseInt x) (Integer/parseInt y))
        collected (collect player)]
    (-> (if (some? collected)
          (do (update-collectables-game-state collected)
              (assoc player :score (+ 1 (:score player))))
          player)
        (update-game-state channel assoc))))

(defn disconnect! [channel status]
  (println "channel closed:" status)
  (update-game-state channel))

(defn connect! [channel windowH windowW]
  (println "Open new connection")
  (update-game-state (new-player windowH windowW) channel))

(defn process-message [channel message]
  (let [json-data (json/parse-string message keyword)]
    (if (and (:height json-data) (:width json-data))
      (connect! channel (:height json-data) (:width json-data))
      (handle-msg! channel message))))

(defn ws-handler [request]
  (with-channel request channel
                (on-close channel (partial disconnect! channel))
                (on-receive channel (fn [data]
                                      (process-message channel data)) )))

(def websocket-routes
  (GET "/" [] ws-handler) )

(defn -main [& {:as args}]
  (show-rand-ex)
  (run-server websocket-routes {:port 8080}))