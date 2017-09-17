(ns the-iron-council.snow
  (:require [play-clj.core :refer [bundle color pixmap! pixmap* pixmap-format sound]]
            [play-clj.g2d :refer [texture]]
            [the-iron-council.common :as c]))

(def snow-count 200)
(def snow-speed (c/screen-to-world -1.5))
(def off-screen-padding 5)

(defn create-snow-texture [c]
  (let [pix-map (pixmap* 1 1 (pixmap-format :r-g-b-a8888))]
    (doto pix-map
      (pixmap! :set-color c)
      (pixmap! :fill-rectangle 0 0 1 1))
    (texture pix-map)))

(defn create-flake [x y flake]
  (assoc (texture flake)
    :width (c/screen-to-world 1) :height (c/screen-to-world 1)
    :x x :y y
    :snow? true
    :id :snow
    :render-layer 1
    :speed snow-speed))

(defn create-snow []
  (let [shades (range 0.5 0.99 0.1)
        shades-list (for [x shades] (color x x x 1))
        snow-textures (map (fn [c] (create-snow-texture c)) shades-list)]
    (for [count (range snow-count)]
      (create-flake (c/screen-to-world (rand-int c/game-width)) 
                    (c/screen-to-world (rand-int c/game-height)) 
                    (rand-nth snow-textures)))))
      

(defn move-snow [screen {:keys [:y :speed] :as entity}]
  (let [new-y (if (< (+ y speed) 0)
                  (+ (c/screen-to-world off-screen-padding) 
                     (c/screen-to-world c/game-height))
                  (+ y speed))]
    (assoc entity :y new-y)))
