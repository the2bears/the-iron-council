(ns the-iron-council.enemy
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! fixture! fixture-def polygon-shape]]
            [play-clj.math :refer [vector-2]]
            [the-iron-council.common :as c]
            [the-iron-council.particle :as particle]))

(def enemy-width (c/screen-to-world 24))
(def enemy-height (c/screen-to-world 24))

(def enemy-speed-y (c/screen-to-world -1))
(defn create-enemy-body!
  [screen x y a]
  (let [body (add-body! screen (body-def :dynamic
                                         :bullet true))]
;        bullet-vector (vector-2 0 c/bullet-speed :rotate a)]
    (->> (polygon-shape :set-as-box enemy-width enemy-height (vector-2 0 0) a)
         (fixture-def :density 0 :friction 0 :restitution 0 :shape)
         (body! body :create-fixture))
    (doto body
      (body-position!
       x y a)
      (body! :set-linear-velocity 0 enemy-speed-y))
    body))

(defn create-enemy!
  [screen x y a]
  (let [bullet-start-offset-vector (vector-2 c/bullet-half-width c/bullet-half-height :rotate a)
        enemy-shape (shape :filled
                           :set-color (color :green)
                           :rect 0 0 enemy-width enemy-height)]
    (assoc enemy-shape
      :enemy? true
      :render-layer 50
      :body (create-enemy-body! screen x y a)
      :x x
      :y y
      :angle a
      :width enemy-width :height enemy-height)))
