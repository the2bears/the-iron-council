(ns the-iron-council.enemy-test
  (:require [play-clj.core :refer [bundle color pixmap! pixmap* pixmap-format shape update! x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! polygon polygon! rectangle rectangle! vector-2 vector-2!]]
            [the-iron-council.common :as c]
            [the-iron-council.enemy-bullet :as eb]
            [the-iron-council.enemy :as enemy]
            [the-iron-council.utils :as utils]))

(def test-side 16)

(def train-car-width 36)
(def train-car-length 72)
(def train-car-width-adj (c/screen-to-world train-car-width))
(def train-car-length-adj (c/screen-to-world train-car-length))

(def tarmac-entity (atom nil))

(defn- create-test-texture []
  (let [pix-map (pixmap* 32 32 (pixmap-format :r-g-b-a8888))]
    (utils/pix-map-rect pix-map (color :white) 0 0 test-side test-side)
    (texture pix-map :set-region 0 0 test-side test-side)))

(defn create-tarmac-entity []
  (let [pix-map (pixmap* 32 64 (pixmap-format :r-g-b-a8888))]
    (utils/pix-map-rect pix-map (color 0.25 0.25 0.35 1) 0 0 24 64)
    (reset! tarmac-entity (assoc (texture pix-map :set-region 0 0 24 64)
                                 :render-layer 6
                                 :width (c/screen-to-world 24)
                                 :height (c/screen-to-world 64)
                                 :armament? true
                                 :enemy? true
                                 :translate-x (- (/ (c/screen-to-world 24) 2))
                                 :translate-y (- (/ (c/screen-to-world 64) 2))))))

(defn create-textures []
  (create-tarmac-entity))

(defn create-test
 ([screen entities]
  (let [a 0
        uuid (c/uuid)
        translate-x (/ (- (c/screen-to-world test-side)) 2)
        translate-y (/ (- (c/screen-to-world test-side)) 1)
        x (/ c/game-width-adj 2)
        y (* 3 (/ c/game-height-adj 4))
        train-car (-> @enemy/train-car-entity
                      (assoc :x x
                             :y y
                             :angle a
                             :id uuid
                             :test-bundle? true
                             :way-points [[0 0]
                                          [0 (/ (c/screen-to-world test-side) 2)]
                                          [(/ (c/screen-to-world test-side) 2) (/ (c/screen-to-world test-side) 2)]
                                          [(- (/ (c/screen-to-world test-side) 2)) (/ (c/screen-to-world test-side) 2)]
                                          [0 (- (/ (c/screen-to-world test-side) 2))]]))
        tarmac (-> @tarmac-entity
                   (assoc :angle 0
                          ;:x x
                          ;:y y
                          :way-points-index 0)
                   (enemy/position-from-parent train-car))
        test-cannon (-> @enemy/cannon-entity
                        (assoc :angle 180
                               :test-cannon? true
                               :way-points-index 1
                               :collider [(enemy/update-collider x (+ y (/ (c/screen-to-world test-side) 2))
                                                                 0 0 a
                                                                 (/ (c/screen-to-world test-side) 3) (/ (c/screen-to-world test-side) 3))])
                        (enemy/position-from-parent train-car))]
    [train-car tarmac]))) ;test-cannon])))
