(ns the-iron-council.levels
  (:require [the-iron-council.enemy :as enemy]
            [the-iron-council.gunship :as gs]
            [the-iron-council.track :as tr]))

(defn start-level [screen entities]
  (let [car (enemy/create-train-car screen entities)]
    (conj entities car)))

(defn level-one [screen]
  (tr/create-curved-track screen))
