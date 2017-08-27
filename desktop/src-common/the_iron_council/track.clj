(ns the-iron-council.track
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape update! x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [vector-2 vector-2!]]
            [the-iron-council.common :as c]))

(def track-texture (atom nil))
(def track-speed -1)
(def track-speed-adj (c/screen-to-world track-speed))
(def track-width 32)
(def track-height 4)
(def track-tie-color (color 0.14 0.12 0.11 1))
(def track-rail-color (color 0.42 0.44 0.47 1))
(def track-lower-limit (c/screen-to-world -60))

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
           :speed track-speed-adj)))

(defn- track-within-limit
  [limit {:keys [y] :as track-piece}]
  (< (+ y limit) (+ c/game-height 60)))

(defn create-curved-track [screen]
  (let [track (-> (create-track-sequence (/ c/game-width 8) 0 (vector-2 0 12) -1 30)
                  (create-track-sequence 0 5)
                  (create-track-sequence 2 30)
                  (create-track-sequence -2 20)
                  (create-track-sequence 0 10)
                  (create-track-sequence 1 10)
                  (create-track-sequence 0 15)
                  (create-track-sequence 4 10)
                  (create-track-sequence -4 10)
                  (create-track-sequence 0 100))]
    (update! screen :track track)))    

(defn add-tracks [screen entities]
  (let [limit (* (:ticks screen) track-speed)
        new-pieces (take-while (partial track-within-limit limit) (:track screen))
        remaining (drop-while (partial track-within-limit limit) (:track screen))
        new-entities (reduce (fn [acc t](conj acc
                                              (create-track-entity (c/screen-to-world (:x t))
                                                                   (c/screen-to-world (+ (:y t) limit))
                                                                   (- (:a t) 90))))
                             []
                             new-pieces)]
    (update! screen :track remaining)
    (concat entities new-entities)))

(defn move-track
  [screen {:keys [y speed] :as entity}]
  (when (> y track-lower-limit)
    (assoc entity :y (+ y speed))))
