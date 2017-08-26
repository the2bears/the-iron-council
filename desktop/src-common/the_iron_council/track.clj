(ns the-iron-council.track
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [vector-2 vector-2!]]
            [the-iron-council.common :as c]))

(def track-texture (atom nil))
(def track-speed (c/screen-to-world 0));-0.05)
(def track-width 32)
(def track-height 4)
(def track-tie-color (color 0.14 0.12 0.11 1))

(defn- draw-rects [pix-map c x y w h]
  (doto pix-map
    (pixmap! :set-color c)
    (pixmap! :fill-rectangle x y w h)))

(defn- create-track-sequence
  ([{:keys [x y v]} r n]
   (create-track-sequence x y v r n))
  ([x y v r n]
   (loop [x x
          y y
          c n
          acc []
          v v]
     (if (= c 0)
       acc
       (let [new-v (vector-2 (core/x v) (core/y v) :rotate r)
             x (+ x (core/x new-v))
             y (+ y (core/y new-v))]
         (recur x
                y
                (dec c)
                (conj acc {:x x
                           :y y
                           :a (vector-2! new-v :angle)
                           :v new-v})
                new-v))))))
                     
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

(defn create-curved-track []
  (let [track-coords (create-track-sequence (/ c/game-width 2) 0 (vector-2 0 12) -3 10)
        t-coords-s (create-track-sequence (last track-coords) 0 5)
        t-coords-2 (create-track-sequence (last t-coords-s) 3 10)
        snd-track (create-track-sequence (/ c/game-width 2) 0 (vector-2 0 12) 0 25)]
    
    (reduce (fn [acc t](conj acc
                             (create-track-entity (c/screen-to-world (:x t))
                                                  (c/screen-to-world (:y t))
                                                  (- (:a t) 90))))
            []
            (concat track-coords t-coords-s t-coords-2 snd-track))))
(defn update-track
  [screen {:keys [y speed] :as entity}]
  (assoc entity :y (+ y speed)))
