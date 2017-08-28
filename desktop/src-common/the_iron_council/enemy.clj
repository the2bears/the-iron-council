(ns the-iron-council.enemy
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! rectangle rectangle! vector-2]]
            [the-iron-council.common :as c]))




(defn create-test-enemy []
  (let [enemy (shape :filled
                     :set-color (color :blue)
                     :rect (c/screen-to-world -10) (c/screen-to-world -10)
                     (c/screen-to-world 20) (c/screen-to-world 20))
        enemy2 (shape :filled
                      :set-color (color :green)
                      :rect (c/screen-to-world -10) (c/screen-to-world -10)
                      (c/screen-to-world 20) (c/screen-to-world 20))
        x1 (/ c/game-width-adj 2)
        y1 (/ c/game-height-adj 2)
        x2 (/ c/game-width-adj 3)
        y2 (/ c/game-height-adj 2)]
   [(assoc enemy
           :enemy? true
           :x x1
           :y y1
           :collider (rectangle (- x1 (c/screen-to-world 10)) (- y1 (c/screen-to-world 10))
                                (c/screen-to-world 20) (c/screen-to-world 20)))
    (assoc enemy2
           :enemy? true
           :x x2
           :y y2
           :collider (rectangle (- x2 (c/screen-to-world 10)) (- y1 (c/screen-to-world 10))
                                (c/screen-to-world 20) (c/screen-to-world 20)))]))
           
