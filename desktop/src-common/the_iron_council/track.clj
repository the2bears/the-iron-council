(ns the-iron-council.track
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [vector-2]]
            [the-iron-council.common :as c]))

(def track-texture (atom nil))
(def track-speed (c/screen-to-world -0.4))
(def track-width 32)
(def track-height 4)
(def track-tie-color (color 0.5 0.5 0.5 1))

(defn- draw-rects [pix-map c x y w h]
  (doto pix-map
    (pixmap! :set-color c)
    (pixmap! :fill-rectangle x y w h)))

(defn- create-track-texture []
  (let [pix-map (pixmap* track-width track-height (pixmap-format :r-g-b-a8888))]
    (draw-rects pix-map track-tie-color 0 0 track-width track-height)
    (texture pix-map :set-region 0 0 track-width track-height)))

(defn create-track-entity
  [x y a]
  (let [track-texture (cond (nil? @track-texture)
                            (do
                              (reset! track-texture (create-track-texture))
                              @track-texture)
                            :else @track-texture)]
    (assoc track-texture
           :width (c/screen-to-world 32) :height (c/screen-to-world 4)
           :x x :y y :angle a
           :translate-x (c/screen-to-world (- (/ track-width 2)))
           :translate-y (c/screen-to-world (- (/ track-height 2)))
           :track? true :id :track
           :render-layer 1
           :speed track-speed)))

(defn update-track
  [screen {:keys [y speed] :as entity}]
  (assoc entity :y (+ y speed)))
