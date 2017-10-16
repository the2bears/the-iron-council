(ns the-iron-council.levels
  (:require [play-clj.core :refer [update!]]
            [the-iron-council.enemy :as enemy]
            [the-iron-council.gunship :as gs]
            [the-iron-council.track :as tr]))

(defn create-track! [screen entities]
  (tr/create-curved-track screen)
  entities)

(defn start-train [screen entities]
  (into entities (enemy/create-train-car screen entities)))

(def levels {1 {:events [{:level-ticks 120 :fn start-train}]}})

(defn start-level [{:keys [level ticks] :as screen} entities]
  (-> screen
      (update! :level-ticks 0)
      (create-track! entities)))

(defn update-level [{:keys [level level-ticks] :as screen} entities]
  (if level
    (let [level-events (drop-while (fn[m](< (:level-ticks m) level-ticks)) (get-in levels [level :events]))
          event (first level-events)]
      (update! screen :level-ticks (inc level-ticks))
      (if (= level-ticks (:level-ticks event))
        ((:fn event) screen entities)
        entities))
    entities))
