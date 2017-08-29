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
        y2 (/ c/game-height-adj 2)
        line (shape :line
                    :line 0.0 (float (- y1 (c/screen-to-world 10))) (float c/game-width-adj) (float (- y1 (c/screen-to-world 10))) (color :lime) (color :lime))
        line2 (shape :line
                     :line 0.0 (float (+ y1 (c/screen-to-world 10))) (float c/game-width-adj) (float (+ y1 (c/screen-to-world 10))) (color :lime) (color :lime))
        rec1 (rectangle x1 y1
                        (c/screen-to-world 20) (c/screen-to-world 20))
        rec2 (rectangle x2 y2
                        (c/screen-to-world 20) (c/screen-to-world 20))]
    (rectangle! rec1 :set-center x1 y1)
    (rectangle! rec2 :set-center x2 y2)
       
    [;(assoc line :line? true)
     ;(assoc line2 :line? true)
     (assoc enemy
           :enemy? true
           :x x1
           :y y1
           :collider rec1)
     (assoc enemy2
            :enemy? true
            :x x2
            :y y2
            :collider rec2)]))
           
