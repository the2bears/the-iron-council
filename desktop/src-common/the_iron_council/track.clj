(ns the-iron-council.track
  (:require [clojure.pprint :as pp]
            [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape update! x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [vector-2 vector-2!]]
            [the-iron-council.common :as c]
            [the-iron-council.enemy :as enemy]))

(def tie-texture (atom nil))
(def rail-texture (atom nil))
(def track-speed -1.5);1
(def track-speed-adj (c/screen-to-world track-speed))
(def track-lower-limit (c/screen-to-world -60))
(def i-points-per-track 9)

(defn- draw-rects [pix-map c x y w h]
  (doto pix-map
    (pixmap! :set-color c)
    (pixmap! :fill-rectangle x y w h)))

(defn- interpolate
  ([x y n]
   (interpolate x y n false))
  ([x y n relative?]
   (let [step-size (/ (- (float y) (float x)) n)]
     (for [step (range n)]
       (+ (* step step-size) (if relative? x 0))))))

(defn- interpolation-points [x x' y y' n]
  (into []
    (map (fn [ix iy] ;ia]
           {:x ix :y iy})
         (interpolate x x' n)
         (interpolate y y' n))))

(defn- create-track-sequence
  ([s r n]
   (let [s (sort-by :y s)
         {:keys [x y v]} (last s)]
     (create-track-sequence x y v r (drop-last s) n)))
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
       (let [a (vector-2! v :angle)
             next-v (vector-2 (core/x v) (core/y v) :rotate r)
             next-x (+ x (core/x next-v))
             next-y (+ y (core/y next-v))
             i-points (interpolation-points (c/screen-to-world x) (c/screen-to-world next-x)
                                            (c/screen-to-world y) (c/screen-to-world next-y)
                                            i-points-per-track)]
         (recur next-x
                next-y
                (dec c)
                (conj acc {:x x
                           :y y
                           :angle a
                           :v v
                           :i-points i-points})
                next-v))))))

(def track-width 32)
(def track-height 4)
(def rail-width 2)
(def upper-buffer 5)
(def side-buffer 4)
(def track-tie-color (color 0.14 0.12 0.11 1))
(def track-rail-color (color 0.42 0.44 0.47 1))

(defn- create-tie-texture []
  (let [pix-map (pixmap* track-width 16 (pixmap-format :r-g-b-a8888))]
    (draw-rects pix-map track-tie-color 0 upper-buffer track-width track-height)
    (texture pix-map :set-region 0 0 track-width 14)))

(defn- create-rails-texture []
  (let [pix-map (pixmap* track-width 16 (pixmap-format :r-g-b-a8888))]
    (draw-rects pix-map track-rail-color side-buffer 0 rail-width 14)
    (draw-rects pix-map track-rail-color (- track-width side-buffer rail-width) 0 rail-width 14)
    (texture pix-map :set-region 0 0 track-width 14)))

(defn create-track-entity
  [x y angle ticks i-points]
  (let [tie-texture (cond (nil? @tie-texture)
                          (do
                            (reset! tie-texture (create-tie-texture))
                            @tie-texture)
                          :else @tie-texture)
        rail-texture (cond (nil? @rail-texture)
                           (do
                             (reset! rail-texture (create-rails-texture))
                             @rail-texture)
                           :else @rail-texture)]
    [(assoc tie-texture
            :width (c/screen-to-world 32)
            :height (c/screen-to-world 14)
            :x x
            :y y
            :angle angle
            :translate-x (c/screen-to-world (- (/ track-width 2)))
            :translate-y (c/screen-to-world (- (/ track-height 2)))
            :track? true
            :id :track
            :at-ticks ticks
            :i-points i-points
            :render-layer 1
            :speed track-speed-adj)
     (assoc rail-texture
            :width (c/screen-to-world 32)
            :height (c/screen-to-world 14)
            :x x
            :y y
            :angle angle
            :translate-x (c/screen-to-world (- (/ track-width 2)))
            :translate-y (c/screen-to-world (- (/ track-height 2)))
            :rail? true
            :id :track
            :i-points i-points
            :render-layer 2
            :speed track-speed-adj)]))

(defn- track-within-limit
  [limit {:keys [y] :as track-piece}]
  (< (+ y limit) (+ c/game-height 60)))


(def t-t (create-track-sequence (/ c/game-width 8) 0 (vector-2 0 12) -0.5 10))
(->> t-t
     (map #(:x %))
     (partition 2 1)
     (map (fn[[a b]](- b a))))

(def n 0.20942887663841248)

(map #(* % n) [1 2 3 4 5 6 7 8 9 10])

(defn create-curved-track [screen]
  (let [track (-> (create-track-sequence (/ c/game-width 8) 0 (vector-2 0 12) 0 5)
                  (create-track-sequence -1 30)
                  (create-track-sequence 0 5)
                  (create-track-sequence 2 30)
                  (create-track-sequence -2 20)
                  (create-track-sequence 0 10)
                  (create-track-sequence 1 10)
                  (create-track-sequence 0 15)
                  (create-track-sequence 4 10)
                  (create-track-sequence -4 10)
                  (create-track-sequence 0 100)
                  (create-track-sequence -2 10)
                  (create-track-sequence 0 20)
                  (create-track-sequence 2 10)
                  (create-track-sequence 0 20))]
   (update! screen :track (sort-by :y track))))

(defn add-tracks [{:keys [ticks] :as screen} entities]
  (let [limit (* ticks track-speed)
        new-pieces (take-while (partial track-within-limit limit) (:track screen))
        remaining (drop-while (partial track-within-limit limit) (:track screen))
        new-entities (reduce (fn [acc t](into acc
                                              (create-track-entity (c/screen-to-world (:x t))
                                                                   (c/screen-to-world (+ (:y t) limit))
                                                                   (- (:angle t) 90)
                                                                   ticks
                                                                   (:i-points t))))
                             []
                             new-pieces)]
    (update! screen :track remaining)
    (into entities new-entities)))

(defn move-track
  [screen {:keys [i-points y speed] :as entity}]
  (when (> y track-lower-limit)
    (assoc entity :y (+ y speed))))
