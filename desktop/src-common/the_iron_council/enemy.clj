(ns the-iron-council.enemy
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape update! x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! polygon polygon! rectangle rectangle! vector-2]]
            [the-iron-council.common :as c]))

(def rec-side (c/screen-to-world 10))
(def rec-offset (- (/ rec-side 2)))

(def poly-length (c/screen-to-world 32))
(def poly-height (c/screen-to-world 64))
(def poly-len-offset (/ poly-length 2))
(def poly-height-offset (/ poly-height 2))

(defn create-test-collider [x y len-offset height-offset]
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
                          :rect (- poly-len-offset) (- poly-height-offset)
                                poly-length poly-height)
        px1 (/ c/game-width-adj 2)
        py1 (- c/game-height-adj (/ c/game-height-adj 4))
        poly-collider (create-test-collider px1 py1 poly-len-offset poly-height-offset)]
    [(assoc poly-enemy
            :enemy? true
            :x px1
            :y py1
            :angle 0
            :collider (polygon (polygon! poly-collider :get-transformed-vertices))
            :collider-len-offset poly-len-offset
            :collider-height-offset poly-height-offset
            :collider-type :poly)]))

(defn create-test-car [screen entities]
  (let [car-shape (shape :filled
                         :set-color (color 0.7 0.7 0.7 1)
                         :rect (- poly-len-offset) (- poly-height-offset)
                               poly-length poly-height)
        current-tracks (sort-by :at-ticks (filter :track? entities))
        x (/ c/game-width-adj 2)
        y c/game-height-adj
        track (last current-tracks)]
    [(assoc car-shape
            :train? true
            :x x
            :y y
            :angle 0
            :render-layer 5
            :track (:at-ticks track)
            :i-point-index 0)]))

(defn move-enemy [screen {:keys [x y angle collider-len-offset collider-height-offset collider-type] :as enemy}]
  (cond (= :poly collider-type)
        (let [dx (c/screen-to-world 0.03)
              dy (- (c/screen-to-world 0.04))
              new-x (+ x dx)
              new-y (+ y dy)
              new-collider (create-test-collider new-x new-y collider-len-offset collider-height-offset)]
          (polygon! new-collider :set-rotation (+ angle 0.15))
          (let [collider (polygon (polygon! new-collider :get-transformed-vertices))]
            (assoc enemy :x (+ x dx) :y (+ y dy) :angle (+ angle 0.15) :collider collider)))
        :else enemy))

(defn- next-track-entity [at-track tracks]
  (let [sorted-tracks (sort-by :at-ticks tracks)
        next-track (first (drop-while #(<= (:at-ticks %) at-track) sorted-tracks))]
;    (prn :next-track-angle (:angle next-track))
    next-track))

(defn move-train [screen entities {:keys [i-point-index track] :as entity}]
  (let [tracks (filter :track? entities)]
    (if-let [track-entity (first (filter #(= track (:at-ticks %)) tracks))]
      (let [i-points (:i-points track-entity)
            current-point (get i-points i-point-index)
            x (+ (:x track-entity) (:x current-point))
            y (+ (:y track-entity) (:y current-point))
            angle (:angle track-entity);angle of track
;            a-c-p (:angle current-point);angle of interpolation points
            get-next (= (dec (count i-points)) i-point-index)
            next-track (next-track-entity track tracks)
            next-a (if (nil? next-track) 0.0 (:angle next-track))
            d-a (- next-a angle)
            angle-offset (* d-a (/ i-point-index (count i-points)))]
;        (prn :x (* x c/s-to-w-divider) :y (* y c/s-to-w-divider) :a-c-p a-c-p :angle (+ angle-offset angle))
        (assoc entity
               :x x :y y :i-point-index (if get-next 0 (inc i-point-index)) :angle (+ angle-offset angle)
               :track (if get-next (:at-ticks next-track) track)))
      entity)))

