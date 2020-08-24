(ns exceptional-monkeys-game-server.core
  (:require
    [clj-uuid :as uuid]
    [cheshire.core :as json]
    [immutant.web :as web]
    [immutant.web.async :as async]
    [immutant.web.middleware :as web-middleware]
    [compojure.route :as route]
    [environ.core :refer (env)]
    [compojure.core :refer (ANY GET defroutes)]
    [ring.util.response :refer (response redirect content-type)])
  (:gen-class))

(def exceptionTypes ["IOException", "DivideByZeroException", "NullPointerException",  "IndexOutOfBoundsException", "InterruptedException", "RuntimeException"])
(def players (atom {}))

(defn broadcast-msg [connections msg]
  (doseq [[con p] connections] (async/send! con (json/generate-string msg {:pretty true}))))

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

  ([player channel edit-func]
   "Edit exsiting player values "
   (-> (swap! players edit-func channel (assoc player :collision false))
       (broadcast-msg player))))

(defn move-player [player moveX moveY windowH windowW]
  (let [x (+ moveX (:x player))
        y (+ moveY (:y player))]
    (if (or (< y 0) (< x 0) (>= x windowW) (>= y windowH))
      (assoc player :collision true :windowW windowW :windowH windowH)
      (assoc player :x x :y y :windowW windowW :windowH windowH))))

(defn new-player []
  {:player?       true
   :id            (str (uuid/v1))
   :x             (rand-int 600)
   :y             (rand-int 300)
   :score         0
   :show          true
   :exceptionType (rand-nth exceptionTypes)
   :color         [(rand-int 256) (rand-int 256) (rand-int 256)]
   :collision     false
   :windowH 0
   :windowW 0})

(def websocket-callbacks
  {:on-open    (fn [channel]
                 (-> (new-player)
                     (update-game-state channel)))
   :on-close   (fn [channel {:keys [code reason]}]
                 (update-game-state channel)
                 (println "close code:" code "reason:" reason))
   :on-message (fn [channel msg]
                 (let [{:keys [height width x y]} (json/parse-string msg keyword)]
                   (-> (move-player (@players channel) (Integer/parseInt x) (Integer/parseInt y) height width)
                       (update-game-state channel assoc))))})

(defroutes routes
           (GET "/" {c :context} (redirect (str c "/index.html")))
           (route/resources "/"))

(defn -main [& {:as args}]
  (web/run
    (-> routes
        (web-middleware/wrap-websocket websocket-callbacks))
    (merge {"host" (env :demo-web-host), "port" 3030} args)))