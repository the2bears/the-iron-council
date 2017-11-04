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

(def tarmac-entity (atom nil))
(def test-entity (atom nil))

(defn launch-test-enemy [_ _ {:keys [x y angle id launch-point enemy-ticks] :or {enemy-ticks 1} :as entity}]
  (when (= 0 (mod enemy-ticks 600))
    (let [launch-vector (vector-2 (first launch-point) (second launch-point) :rotate angle)]
      (assoc @test-entity
             :x (+ x (core/x launch-vector))
             :y (+ y (core/y launch-vector))
             :angle angle
             :parent-id id))))

(defn- create-test-entity []
  (let [pix-map (pixmap* 32 32 (pixmap-format :r-g-b-a8888))]
    (utils/pix-map-rect pix-map (color :white) 0 0 test-side test-side)
    (reset! test-entity (assoc (texture pix-map :set-region 0 0 test-side test-side)
                               :render-layer 16
                               :width (c/screen-to-world test-side)
                               :height (c/screen-to-world test-side)
                               :drone? true
                               :enemy? true
                               :translate-x (- (/ (c/screen-to-world test-side) 2))
                               :translate-y (- (/ (c/screen-to-world test-side) 2))))))

(defn create-tarmac-entity []
  (let [pix-map (pixmap* 32 64 (pixmap-format :r-g-b-a8888))]
    (utils/pix-map-rect pix-map (color 0.25 0.25 0.35 1) 0 0 24 64)
    (reset! tarmac-entity (assoc (texture pix-map :set-region 0 0 24 64)
                                 :render-layer 6
                                 :width (c/screen-to-world 24)
                                 :height (c/screen-to-world 64)
                                 :armament? true
                                 :enemy? true
                                 :launch-point [0 (- (c/screen-to-world 20))]
                                 :launch-length 50
                                 :attack-fn launch-test-enemy
                                 :translate-x (- (/ (c/screen-to-world 24) 2))
                                 :translate-y (- (/ (c/screen-to-world 64) 2))))))

(defn create-textures []
  (create-tarmac-entity)
  (create-test-entity))

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
                             :way-points [[0 0]
                                          [0 (/ (c/screen-to-world test-side) 2)]
                                          [(/ (c/screen-to-world test-side) 2) (/ (c/screen-to-world test-side) 2)]
                                          [(- (/ (c/screen-to-world test-side) 2)) (/ (c/screen-to-world test-side) 2)]
                                          [0 (- (/ (c/screen-to-world test-side) 2))]]))
        tarmac (-> @tarmac-entity
                   (assoc :angle 0
                          :id (c/uuid)
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

(defn handle-test-bundle [screen {:keys [angle ticks] :or {ticks 75} :as entity}]
  (let [right?  (key-pressed? :dpad-right)
        left? (key-pressed? :dpad-left)
        da (cond right? -0.3 left? 0.3 :else 0)]
    (assoc entity :angle (+ angle da) :ticks (inc ticks))))

(defn handle-drone [screen entities {:keys [parent-id ticks] :or {ticks 1} :as entity}]
  (let [parent (first (filter #(= parent-id (:id %)) entities))
        x (:x parent)
        y (:y parent)
        angle (:angle parent)
        launch-point (:launch-point parent)
        launch-length (:launch-length parent)
        launch-point-vector (vector-2 (first launch-point) (second launch-point) :rotate angle)
        launch-vector (vector-2 0 (c/screen-to-world 1.2) :rotate (:angle parent))
        dx (core/x launch-vector)
        dy (core/y launch-vector)
        current-launch-length (* ticks 1.2)]
    (if (< current-launch-length launch-length)
      (assoc entity
             :x (+ x (core/x launch-point-vector) (* ticks dx))
             :y (+ y (core/y launch-point-vector) (* ticks dy))
             :dx dx
             :dy dy
             :angle angle
             :ticks (inc ticks))
      (let [turn-around (bh/change-direction :sx dx :sy dy :ta 90 :min-ticks (inc ticks) :max-ticks (+ (inc ticks) 60))
            slow-down (bh/change-speed :sx 0 :sy (c/screen-to-world 1.2) :tx 0 :ty (c/screen-to-world 2.4) :min-ticks (+ (inc ticks) 60) :max-ticks (+ (inc ticks) 90))
            continue (bh/continue)
            oob-wrapper (fn[wrapped-fn]
                          (fn[entity]
                            (when-let [entity (wrapped-fn entity)]
                              ;(prn :oob-wrapper :ticks (:ticks entity))
                              entity)))]
        (assoc entity
               :drone? false
               :guided? true
               :x (+ x (core/x launch-point-vector) (* ticks dx))
               :y (+ y (core/y launch-point-vector) (* ticks dy))
               :dx dx
               :dy dy
               :angle angle
               :ticks (inc ticks)
               :bullet-hell-fn (some-fn (oob-wrapper turn-around) (oob-wrapper slow-down) continue))))))
