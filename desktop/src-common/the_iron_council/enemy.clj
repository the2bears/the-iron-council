(ns the-iron-council.enemy
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape update! x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! polygon polygon! rectangle rectangle! vector-2]]
            [the-iron-council.common :as c]
            [the-iron-council.utils :as utils]))

(def rec-side (c/screen-to-world 10))
(def rec-offset (- (/ rec-side 2)))

(def train-car-width 36)
(def train-car-length 72)
(def train-car-width-adj (c/screen-to-world train-car-width))
(def train-car-length-adj (c/screen-to-world train-car-length))
(def train-car-width-offset (/ train-car-width-adj 2))
(def train-car-length-offset (/ train-car-length-adj 2))

(def train-car-texture (atom nil))
(def darkest (color :black))
(def dark (color 0 29/255 71/255 1))
(def dark-highlight (color 55/255 75/255 91/255 1))

(def train-rects [[darkest [0 0 train-car-width train-car-length]]
                  ;[dark-highlight [1 1 34 70]]
                  ;[dark [2 2 32 68]]
                  [dark [1 1 34 70]]])

(defn- train-bolt-strip [pix-map x y w h]
  (doto pix-map
    (utils/pix-map-rect dark x y w h)
    (utils/pix-map-rect dark-highlight x (+ y (dec h)) w 1)
    (utils/pix-map-rect darkest x y w 1))
  (doseq [x (range x w 4)]
    (utils/pix-map-rect pix-map darkest x (inc y) 1 1)))

(defn- create-train-car-texture []
  (let [pix-map (pixmap* 128 128 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set train-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (utils/pix-map-rect pix-map (first color-set) x y w h)))
    (doseq [y [16 34 52]]
      (train-bolt-strip pix-map 0 y 36 4))
    (texture pix-map :set-region 0 0 train-car-width train-car-length)))


(defn- create-collider [x y len-offset height-offset]
  (let [poly-verts (float-array [(- x len-offset) (- y height-offset)
                                 (+ x len-offset) (- y height-offset)
                                 (+ x len-offset) (+ y height-offset)
                                 (- x len-offset) (+ y height-offset)])
        poly (polygon poly-verts)]
    (polygon! poly :set-origin x y)
    poly))

(defn create-test-enemy []
  (let [poly-enemy (shape :filled
                          :set-color (color 0.5 0.5 0.5 1)
                          :rect (- train-car-width-offset) (- train-car-length-offset)
                                train-car-width-adj train-car-length-adj)
        px1 (/ c/game-width-adj 2)
        py1 (- c/game-height-adj (/ c/game-height-adj 4))
        poly-collider (create-collider px1 py1 train-car-width-offset train-car-length-offset)]
    [(assoc poly-enemy
            :enemy? true
            :x px1
            :y py1
            :angle 0
            :collider (polygon (polygon! poly-collider :get-transformed-vertices))
            :collider-len-offset train-car-width-offset
            :collider-height-offset train-car-length-offset
            :collider-type :poly)]))

(defn create-train-car
 ([screen entities]
  (let [current-tracks (sort-by :at-ticks (filter :track? entities))
        track (:at-ticks (last current-tracks))]
    (create-train-car screen entities track)))
 ([screen entities track]
  (let [current-tracks (sort-by :at-ticks (filter :track? entities))
        track-entity (first (filter #(= track (:at-ticks %)) current-tracks))
        i-points (:i-points track-entity)
        i-point-index 0
        current-point (get i-points i-point-index)
        x (+ (:x track-entity) (:x current-point))
        y (+ (:y track-entity) (:y current-point))
        angle (:angle track-entity);angle of track
        train-car (cond (nil? @train-car-texture)
                        (do
                          (reset! train-car-texture (create-train-car-texture))
                          @train-car-texture)
                        :else @train-car-texture)
        car-collider (create-collider x y train-car-width-offset train-car-length-offset)]
    (assoc train-car
           :train? true
           :enemy? true
           :x (:x track-entity)
           :y (:y track-entity)
           :angle (:angle track-entity)
           :width train-car-width-adj
           :height train-car-length-adj
           :translate-x (- train-car-width-offset)
           :translate-y (- train-car-length-offset)
           :front? true
           :render-layer 5
           :track track
           :i-point-index i-point-index
           :collider (polygon (polygon! car-collider :get-transformed-vertices))
           :collider-type :poly))))

(defn move-enemy [screen {:keys [x y angle collider-len-offset collider-height-offset collider-type] :as enemy}]
  (cond (= :poly collider-type)
        (let [dx (c/screen-to-world 0.03)
              dy (- (c/screen-to-world 0.04))
              new-x (+ x dx)
              new-y (+ y dy)
              new-collider (create-collider new-x new-y collider-len-offset collider-height-offset)]
          (polygon! new-collider :set-rotation (+ angle 0.15))
          (let [collider (polygon (polygon! new-collider :get-transformed-vertices))]
            (assoc enemy :x (+ x dx) :y (+ y dy) :angle (+ angle 0.15) :collider collider)))
        :else enemy))

(defn- next-track-entity [at-track tracks]
  (let [sorted-tracks (sort-by :at-ticks tracks)
        next-track (first (drop-while #(<= (:at-ticks %) at-track) sorted-tracks))]
    next-track))

(defn move-train [screen entities {:keys [i-point-index track] :as entity}]
  (let [tracks (filter :track? entities)]
    (if-let [track-entity (first (filter #(= track (:at-ticks %)) tracks))]
      (let [top-tracks (drop-while #(>= track (:at-ticks %)) tracks)
            i-points (:i-points track-entity)
            current-point (get i-points i-point-index)
            x (+ (:x track-entity) (:x current-point))
            y (+ (:y track-entity) (:y current-point))
            angle (:angle track-entity);angle of track
            get-next (= (dec (count i-points)) i-point-index)
            next-track (next-track-entity track tracks)
            next-a (if (nil? next-track) 0.0 (:angle next-track))
            d-a (- next-a angle)
            angle-offset (* d-a (/ i-point-index (count i-points)))
            new-collider (create-collider x y train-car-width-offset train-car-length-offset)]
        (polygon! new-collider :set-rotation (+ angle angle-offset))
        (let [entity (assoc entity
                            :x x :y y :i-point-index (if get-next 0 (inc i-point-index)) :angle (+ angle-offset angle)
                            :track (if get-next (:at-ticks next-track) track) :collider (polygon (polygon! new-collider :get-transformed-vertices)))]
          (if (and (= 8 (count top-tracks)) (:front? entity))
            [(assoc entity :front? false) (create-train-car screen entities (:at-ticks (last top-tracks)))]
            entity)))
      entity)))

