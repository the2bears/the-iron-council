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
(def poly-hght-offset (/ poly-height 2))

(defn create-test-enemy []
  (let [poly-enemy (shape :filled
                          :set-color (color :red)
                          :rect (- poly-len-offset) (- poly-hght-offset)
                                poly-length poly-height)

        enemy (shape :filled
                     :set-color (color :blue)
                     :rect rec-offset rec-offset
                     rec-side rec-side)
        enemy2 (shape :filled
                      :set-color (color :green)
                      :rect rec-offset rec-offset
                      rec-side rec-side)
        px1 (/ c/game-width-adj 2)
        py1 (- c/game-height-adj (/ c/game-height-adj 4))
        poly-verts (float-array [(- px1 poly-len-offset) (- py1 poly-hght-offset)
                                 (+ px1 poly-len-offset) (- py1 poly-hght-offset)
                                 (+ px1 poly-len-offset) (+ py1 poly-hght-offset)
                                 (- px1 poly-len-offset) (+ py1 poly-hght-offset)])
        x1 (/ c/game-width-adj 2)
        y1 (/ c/game-height-adj 2)
        x2 (/ c/game-width-adj 3)
        y2 (/ c/game-height-adj 2)
        poly-collider (polygon poly-verts)
        rec1 (rectangle x1 y1
                        rec-side rec-side)
        rec2 (rectangle x2 y2
                        rec-side rec-side)]
        
    (rectangle! rec1 :set-center x1 y1)
    (rectangle! rec2 :set-center x2 y2)
    (polygon! poly-collider :set-origin px1 py1)
    [(assoc poly-enemy
            :enemy? true
            :x px1
            :y py1
            :angle 0
            :collider (polygon (polygon! poly-collider :get-transformed-vertices))
            :collider-type :poly
            :collider-half-width poly-len-offset
            :collider-half-height poly-hght-offset)
     (assoc enemy
           :enemy? true
           :x x1
           :y y1
           :collider rec1
           :collider-type :rect)
     (assoc enemy2
            :enemy? true
            :x x2
            :y y2
            :collider rec2
            :collider-type :rect)]))
           

(defn move-enemy [screen {:keys [x y angle collider collider-type] :as enemy}]
  (cond (= :poly collider-type);translate, then set origin, then rotate
        (do
          (polygon! collider :set-origin x y)
          (polygon! collider :set-rotation 0.15)
          (let [new-collider (polygon (polygon! collider :get-transformed-vertices))]
            (assoc enemy :angle (mod (+ angle 0.15) 360) :collider new-collider)))
        :else enemy))
        
