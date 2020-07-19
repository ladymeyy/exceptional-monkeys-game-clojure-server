(ns exceptional-monkeys-game-server.core
  (:require
    [clj-uuid :as uuid]
    [cheshire.core :as json]
    [immutant.web             :as web]
    [immutant.web.async       :as async]
    [immutant.web.middleware  :as web-middleware]
    [compojure.route          :as route]
    [environ.core             :refer (env)]
    [compojure.core           :refer (ANY GET defroutes)]
    [ring.util.response       :refer (response redirect content-type)])
  (:gen-class))

(def exceptionTypes ["IOException", "DivideByZeroException", "NullPointerException", "ArithmeticException", "FileNotFoundException", "IndexOutOfBoundsException",
                     "InterruptedException", "ClassNotFoundException", "NoSuchFieldException", "NoSuchMethodException", "RuntimeException"])
(def collectables (atom []))
(def players (atom []))
(def channel-store (atom []))

(defn channel->player [channel] (some #(when (= (:channel %) channel) %) @players))

(defn filter-player [channel] (into [] (filter #(not= (:channel %) channel) @players)))

(defn update-player-in-players [player] (reset! players (conj (filter-player (:channel player)) player) ) )

(defn broadcast-msg [chnls msg]
  (if (vector? chnls)
    (doseq [ch chnls]
      (async/send! ch (json/generate-string (into {} (filter #(not= (key %) :channel) msg)) {:pretty true})))
    (async/send! chnls (json/generate-string (into {} (filter #(not= (key %) :channel) msg)) {:pretty true}))))

(defn move [ch move-x move-y]
  (let [player (channel->player ch)
        x (+ move-x (:x player))
        y (+ move-y (:y player))]
    (if (or (< y 0) (< x 0) (>= x (:windowW player)) (>= y (:windowH player)))
      (broadcast-msg @channel-store (assoc player :collision true)))
    (do
      (update-player-in-players (assoc player :x x :y y))
      (broadcast-msg @channel-store (assoc player :x x :y y)))))

(defn handle-incoming-msg [ch msg]
  (let [parsed-msg (json/parse-string msg keyword)
        {:keys [height width x y] } parsed-msg]
    (cond
      (and height width) (update-player-in-players (assoc (channel->player ch) :windowH height :windowW width))
      (and x y) (move ch (Integer/parseInt x) (Integer/parseInt y)))))

(defn remove-player [channel]
  (broadcast-msg @channel-store (assoc (channel->player channel) :show false))
  (reset! channel-store (filter #(not= % channel) @channel-store))
  (reset! players (filter-player channel)))

(defn add-new-player [ch]
  (let [new-player {:player? true
                    :id (str (uuid/v1))
                    :x (rand-int 600)
                    :y (rand-int 300)
                    :score 0
                    :show true
                    :exceptionType "IoException"            ;todo add exceptions
                    :color [(rand-int 256) (rand-int 256) (rand-int 256)]
                    :collision false
                    :channel ch
                    :windowH 0
                    :windowW 0}]
    (broadcast-msg ch (assoc new-player :self? true))       ;send player to self
    (doseq [p @players] (broadcast-msg ch p))               ;send to self existing players
    (broadcast-msg @channel-store new-player)               ;send new player to all existing players
    (swap! players conj new-player)                         ;insert new player to players
    (swap! channel-store conj ch)))                         ;insert to channels list

(def websocket-callbacks
  "WebSocket callback functions"
  {:on-open    add-new-player
   :on-close   (fn [channel {:keys [code reason]}]
                 (remove-player channel)
                 (println "close code:" code "reason:" reason))
   :on-message handle-incoming-msg})

(defroutes routes
           (GET "/" {c :context} (redirect (str c "/index.html")))
           (route/resources "/"))

(defn -main [& {:as args}]
  (web/run
    (-> routes
        ;; wrap the handler with websocket support
        ;; websocket requests will go to the callbacks, ring requests to the handler
        (web-middleware/wrap-websocket websocket-callbacks))
    (merge {"host" (env :demo-web-host), "port" 8080}
           args)))
