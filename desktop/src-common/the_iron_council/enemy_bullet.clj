(ns the-iron-council.enemy-bullet
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! polygon polygon! vector-2]]
            [the-iron-council.common :as c]
            [the-iron-council.utils :as utils]))


(def bullet-texture (atom nil))
(def bullet-rects [[(color 1.0 0 1.0 1) [2 0 4 8 1 1 6 6 0 2 8 4]]
                   [(color 1.0 0.5 1.0 1) [2 1 4 6 1 2 6 4]]
                   [(color :white) [3 1 2 6 2 2 4 4 1 3 6 2]]])
(def bullet-speed (c/screen-to-world 0.1))

(defn- simple-movement
  ([velocity]
   (simple-movement (core/x velocity) (core/y velocity)))
  ([dx dy]
   (fn [{:keys [x y] :as bullet}]
      (assoc bullet :x (+ x dx)
                    :y (+ y dy)))))

(defn- create-bullet-texture []
  (let [pix-map (pixmap* 8 8 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set bullet-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (utils/pix-map-rect pix-map (first color-set) x y w h)))
    (texture pix-map :set-region 0 0 8 8)))

(defn fire-cannon!
  [screen x y a]
  (let [bullet-velocity-vector (vector-2 0 bullet-speed :rotate a)
        bullet (cond (nil? @bullet-texture)
                     (do
                       (reset! bullet-texture (create-bullet-texture))
                       @bullet-texture)
                     :else @bullet-texture)]
    (assoc bullet
           :enemy-bullet? true
           :render-layer 60
           :x (c/screen-to-world x)
           :y (c/screen-to-world y)
           :angle a
           :width (c/screen-to-world 8)
           :height (c/screen-to-world 8)
           :translate-x (- (c/screen-to-world 4))
           :translate-y (- (c/screen-to-world 4))
           :bullet-hell-fn (simple-movement bullet-velocity-vector))))

(defn handle-bullet [screen {:keys [bullet-hell-fn] :as entity}]
  (let [move-fn bullet-hell-fn]
    (move-fn entity)))
