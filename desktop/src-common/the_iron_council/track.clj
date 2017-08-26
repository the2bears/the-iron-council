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
(def track-rail-color (color 0.42 0.44 0.47 1))

(defn- draw-rects [pix-map c x y w h]
  (doto pix-map
    (pixmap! :set-color c)
    (pixmap! :fill-rectangle x y w h)))

(defn- create-track-sequence
  ([s r n]
   (let [{:keys [x y v]} (last s)]
     (create-track-sequence x y v r s n)))
  ([x y v r n]
   (create-track-sequence x y v r [] n))
  ([x y v r acc n]
   (loop [x x
          y y
          c n
          acc acc
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
  (let [pix-map (pixmap* track-width 16 (pixmap-format :r-g-b-a8888))]
    (draw-rects pix-map track-tie-color 0 5 track-width track-height)
    (draw-rects pix-map track-rail-color track-height 0 2 14)
    (draw-rects pix-map track-rail-color 26 0 2 14)
    (texture pix-map :set-region 0 0 track-width 14)))

(defn create-track-entity
  [x y a]
  (let [track-texture (cond (nil? @track-texture)
                            (do
                              (reset! track-texture (create-track-texture))
                              @track-texture)
                            :else @track-texture)]
    (assoc track-texture
           :width (c/screen-to-world 32) :height (c/screen-to-world 14)
           :x x :y y :angle a
           :translate-x (c/screen-to-world (- (/ track-width 2)))
           :translate-y (c/screen-to-world (- (/ track-height 2)))
           :track? true :id :track
           :render-layer 1
           :speed track-speed)))

(defn create-curved-track []
  (let [track (-> (create-track-sequence (/ c/game-width 2) 0 (vector-2 0 12) -3 10)
                  (create-track-sequence 0 5)
                  (create-track-sequence 3 10))]
                  ;(create-track-sequence (/ c/game-width 2) 0 (vector-2 0 12) 0 25))]
    (reduce (fn [acc t](conj acc
                             (create-track-entity (c/screen-to-world (:x t))
                                                  (c/screen-to-world (:y t))
                                                  (- (:a t) 90))))
            []
            track))) ;(concat track-coords t-coords-s t-coords-2 snd-track))))
(defn update-track
  [screen {:keys [y speed] :as entity}]
  (assoc entity :y (+ y speed)))
