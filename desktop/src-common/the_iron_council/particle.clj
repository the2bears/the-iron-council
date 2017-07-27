(ns the-iron-council.particle
  (:require [play-clj.core :refer [bundle color pixmap! pixmap* pixmap-format update! x y]]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [vector-2 vector-2!]]
            [the-iron-council.common :as c]))

(def shell-casing-texture (atom nil))
(def spark-speed-slow (c/screen-to-world 0.3))

(defn- random-velocity [a]
  (let [random-angle-offset (- (rand-int 20) 10)]
    (vector-2 0 spark-speed-slow :rotate (+ 180 a random-angle-offset))))

(defn- random-ticks []
  (+ 15 (rand-int 25)))

(defn- create-shell-casing-texture []
  (let [pix-map (pixmap* 2 2 (pixmap-format :r-g-b-a8888))]
    (doto pix-map
      (pixmap! :set-color c/gatling-shell-casing-color)
      (pixmap! :fill-rectangle 0 0 2 2))
    (texture pix-map)))

(defn create-shell-casing [x y angle]
  (let [shell-casing (cond (nil? @shell-casing-texture)
                           (do
                             (reset! shell-casing-texture (create-shell-casing-texture))
                             @shell-casing-texture)
                           :else @shell-casing-texture)]
    (assoc shell-casing
           :width (c/screen-to-world 1)
           :height (c/screen-to-world 1)
           :x x :y y
           :shell-casing? true
           :id :shell-casing
           :render-layer 72
           :velocity (random-velocity angle)
           :ttl (random-ticks))))
         
(defn update-shell-casing [screen {:keys [:ttl :velocity] :as entity}]
  (cond (> ttl 0) (assoc entity :x (+ (:x entity) (x velocity)) :y (+ (:y entity) (y velocity)) :ttl (- ttl 1))
        :else nil))
