(ns the-iron-council.enemy-test
  (:require [play-clj.core :refer [bundle color key-pressed? pixmap! pixmap* pixmap-format shape update! x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! polygon polygon! rectangle rectangle! vector-2 vector-2!]]
            [the-iron-council.bullet-hell :as bh]
            [the-iron-council.common :as c]
            [the-iron-council.enemy-bullet :as eb]
            [the-iron-council.enemy :as enemy]
            [the-iron-council.utils :as utils]))

(def test-side 16)

(def train-car-width 36)
(def train-car-length 72)
(def train-car-width-adj (c/screen-to-world train-car-width))
(def train-car-length-adj (c/screen-to-world train-car-length))

(defn create-test
 ([screen entities]
  (let [a 0
        uuid (c/uuid)
        translate-x (/ (- (c/screen-to-world test-side)) 2)
        translate-y (/ (- (c/screen-to-world test-side)) 1)
        x (/ c/game-width-adj 2)
        y (/ c/game-height-adj 4)
        train-car (-> @enemy/train-car-entity
                      (assoc :x x
                             :y y
                             :angle a
                             :id uuid
                             :test-bundle? true
                             :way-points [[0 (/ (c/screen-to-world test-side) 2)]]))
        test-cannon (-> @enemy/cannon-entity
                        (assoc :angle 180
                               :test-cannon? true
                               :way-points-index 0
                               :collider [(enemy/update-collider x (+ y (/ (c/screen-to-world test-side) 2))
                                                                 0 0 a
                                                                 (/ (c/screen-to-world test-side) 3) (/ (c/screen-to-world test-side) 3))])
                        (enemy/position-from-parent train-car))]
    [train-car test-cannon])))

(defn handle-test-bundle [screen {:keys [angle ticks] :or {ticks 75} :as entity}]
  (let [right?  (key-pressed? :dpad-right)
        left? (key-pressed? :dpad-left)
        da (cond right? -0.3 left? 0.3 :else 0)]
    (assoc entity :angle (+ angle da) :ticks (inc ticks))))

