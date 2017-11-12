(ns the-iron-council.drone
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
(def drone-entity (atom nil))

(defn- launch-drone [_ _ {:keys [x y angle id launch-point enemy-ticks] :or {enemy-ticks 1} :as entity}]
  (when (or (= 60 enemy-ticks)
            (= 120 enemy-ticks)
            (= 180 enemy-ticks)
            (= 240 enemy-ticks))
    (let [launch-vector (vector-2 (first launch-point) (second launch-point) :rotate angle)]
      (assoc @drone-entity
             :x (+ x (core/x launch-vector))
             :y (+ y (core/y launch-vector))
             :wave-id (quot enemy-ticks 60)
             :angle angle
             :parent-id id))))

(defn- create-drone-entity []
  (let [pix-map (pixmap* 32 32 (pixmap-format :r-g-b-a8888))]
    (utils/pix-map-rect pix-map (color :white) 0 0 test-side test-side)
    (reset! drone-entity (assoc (texture pix-map :set-region 0 0 test-side test-side)
                               :render-layer 16
                               :width (c/screen-to-world test-side)
                               :height (c/screen-to-world test-side)
                               :drone? true
                               :enemy? true
                               :drone-state :launch
                               :oob-exempt? true
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
                                 :launch-length 48
                                 :attack-fn launch-drone
                                 :translate-x (- (/ (c/screen-to-world 24) 2))
                                 :translate-y (- (/ (c/screen-to-world 64) 2))))))

(defn create-textures []
  (create-tarmac-entity)
  (create-drone-entity))

(defn add-drone-carrier
  [train-car screen entities]
  (let [train-car (assoc train-car :way-points [[0 (/ (c/screen-to-world test-side) 2)]])
        tarmac (-> @tarmac-entity
                   (assoc :angle 0
                          :id (c/uuid)
                          :way-points-index 0)
                   (enemy/position-from-parent train-car))]
    [train-car
     tarmac]))

(defn create-drone-carrier
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
                             :test-bundle? true))]
    (add-drone-carrier train-car screen entities))))

(defn- oob-then-wait [wrapped-fn limit]
  (fn[entity]
    (if-let [entity (wrapped-fn entity)]
      (if (> (:y entity) limit)
        (assoc entity
               :drone-state :wait
               :bullet-hell-fn (bh/wait))
        entity))))

(defn- handle-drone-launch [screen entities {:keys [parent-id ticks] :or {ticks 1} :as entity}]
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
            continue (bh/continue)] 
        (assoc entity
               :drone-state :guided
               :x (+ x (core/x launch-point-vector) (* ticks dx))
               :y (+ y (core/y launch-point-vector) (* ticks dy))
               :dx dx
               :dy dy
               :angle angle
               :ticks (inc ticks)
               :bullet-hell-fn (oob-then-wait (some-fn turn-around slow-down continue)
                                              (+ c/game-height-adj (c/screen-to-world 10))))))))

(defn- handle-drone-wait [{:keys [dx dy ticks wait-ticks wave-id] :or {wait-ticks 1} :as entity}]
  (if (< wait-ticks 90)
    (assoc entity :wait-ticks (inc wait-ticks))
    (let [_ (prn :handle-drone-wait :attack-run :ticks ticks :dx dx :dy dy)
          sy (- (c/screen-to-world 3))
          attack-run (bh/linear-movement
                      :dx 0
                      :dy (- (c/screen-to-world 1.5)))
          att-run (bh/change-speed
                   :sy sy
                   :ty (/ sy 20)
                   :min-ticks ticks
                   :max-ticks (+ ticks 60))
          att-run2 (bh/change-speed
                    :sy (/ sy 20)
                    :ty sy
                    :min-ticks (+ ticks 60)
                    :max-ticks (+ ticks 120))
          continue (bh/continue)
          x (* wave-id (/ c/game-width-adj 5))]
      (assoc entity
             :x x
             :dx 0
             :dy sy
             :drone-state :guided
             :angle 180
             :bullet-hell-fn (some-fn att-run eb/fire-tri-bullets att-run2 continue))))) ; (some-fn attack-run)))))

(defn handle-drone [screen entities {:keys [drone-state] :as entity}]
  (case drone-state
    :launch
    (handle-drone-launch screen entities entity)
    :guided
    (eb/handle-bullet screen entity)
    :wait
    (handle-drone-wait entity)
    :default entity))

