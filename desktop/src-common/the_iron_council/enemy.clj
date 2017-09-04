(ns the-iron-council.enemy
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! polygon polygon! rectangle rectangle! vector-2]]
            [the-iron-council.common :as c]))



(def rec-side (c/screen-to-world 10))
(def rec-offset (- (/ rec-side 2)))

(def poly-length (c/screen-to-world 40))
(def poly-height (c/screen-to-world 20))
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

(defn move-enemy [screen {:keys [x y angle collider-len-offset collider-height-offset collider-type] :as enemy}]
  (cond (= :poly collider-type)
        (let [dx (c/screen-to-world 0.06)
              dy (- (c/screen-to-world 0.1))
              new-x (+ x dx)
              new-y (+ y dy)
              new-collider (create-test-collider new-x new-y collider-len-offset collider-height-offset)]
          (polygon! new-collider :set-rotation (+ angle 0.15))
          (let [collider (polygon (polygon! new-collider :get-transformed-vertices))]
            (assoc enemy :x (+ x dx) :y (+ y dy) :angle (+ angle 0.15) :collider collider)))
        :else enemy))
        
