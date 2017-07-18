(ns the-iron-council.gunship
  (:require [pixel-ships.core :as psc :refer :all]
            [pixel-ships.bollinger :as bollinger :refer :all]
            [the-iron-council.common :as c]
            [play-clj.core :refer [add-timer! bundle shape color key-pressed? pixmap! pixmap* pixmap-format screen! update! x y]]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer :all]
            [play-clj.math :refer [vector-2 vector-2!]]))

(def p-per-r 1)

(defn- hsv-to-rgb
                                        ;Convert hsv to rgb
                                        ;Inputs are floats 0<i<1
  ([[hue saturation value alpha]]
   (if (= saturation 0)
     [value value value alpha]
     (let [hue2 (cond (= 1.0 hue) 0.0
                      :else hue)
           h (int (* hue2 6.0))
           f (- (* hue2 6.0) h)
           p (* value (- 1 saturation))
           q (* value (- 1 (* f saturation)))
           t (* value (- 1 (* (- 1 f) saturation)))]
       (case h
         0 [value t p alpha]
         1 [q value p alpha]
         2 [p value t alpha]
         3 [p q value alpha]
         4 [t p value alpha]
         [value p q alpha])))))

(defn- play-clj-color
  ([{:keys [h s v]}]
   (let [[r g b a] (hsv-to-rgb [h s v 1])]
     (play-clj-color r g b a)))
  ([r g b a]
   (color r g b a)))

(defn- draw-rect-pixelmap [pix-map {:keys [x y color]}]
  (let [c (play-clj-color color)]
    (doto pix-map
      (pixmap! :set-color c)
      (pixmap! :fill-rectangle x y p-per-r p-per-r))))

(defn- create-pixel-map-list
  ([seed c-model]
   (let [ship-map (psc/color-pixel-ship (psc/create-pixel-ship (assoc c/gunship-model :seed seed)) c-model)
         tags (keys (:pixels ship-map))
         pixels (:pixels ship-map)
         shape-builder (fn[s] (reduce (fn[acc n] (conj acc n)) [] s))]
     (reduce (fn[acc tag](concat (shape-builder (tag pixels)) acc)) [] tags))))

(defn create-pixel-ship-texture
  ([]
   (create-pixel-ship-texture (rand-int Integer/MAX_VALUE)))
  ([seed]
   (create-pixel-ship-texture seed bollinger/color-scheme))
  ([seed c-scheme]
   (let [pixel-map-list (create-pixel-map-list seed c-scheme)
         pix-map (pixmap* 16 32 (pixmap-format :r-g-b-a8888))]
     (doseq [pixel pixel-map-list] (draw-rect-pixelmap pix-map pixel))
     (assoc (texture pix-map) :seed seed))))

(defn- create-ship-body!
  [screen]
  (let [body (add-body! screen (body-def :dynamic))
        ship-shape (polygon-shape :set-as-box (c/screen-to-world c/ship-b2d-width) (c/screen-to-world c/ship-b2d-height) (vector-2 0 0) 0)]
    (->> ship-shape
         (fixture-def :density 1 :friction 0 :restitution 1 :shape)
         (body! body :create-fixture))
    (.dispose ship-shape)
    body))

(defn create-ship-entity!
  ([screen]
   (let [pixel-ship (create-pixel-ship-texture Long/MAX_VALUE c/gunship-color-scheme)]
     (doto (assoc pixel-ship
             :body (create-ship-body! screen)
             :width (c/screen-to-world 16) :height (c/screen-to-world 32)
             :id :gunship
             :gunship? true
             :render-layer 90
             :translate-x (- (c/screen-to-world c/ship-mp-xoffset))
             :translate-y (- (c/screen-to-world c/ship-mp-yoffset)))
       (body-position! (c/screen-to-world (/ c/game-width 2)) c/ship-y-start 0)
       (body! :set-linear-velocity 0 0)))))

(defn- angle-reset [angle]
  (let [ccw? (< angle 0)
        yaw-reset-fn (if ccw? + -)
        yaw-reset-amt (if ccw? (if (< (- angle) c/yaw-reset-amt) (- angle) c/yaw-reset-amt)
                          (if (< angle c/yaw-reset-amt) angle c/yaw-reset-amt))]
    (yaw-reset-fn angle yaw-reset-amt)))

(defn- angle-reset-body [{:keys [:x :y :angle] :as entity}]
  (body-position! entity x y (angle-reset angle))
  entity)

(defn- just-a [a b]
  a)

(defn- move [{:keys [:x :y :angle] :as entity} x-dir x-delta y-dir y-delta]
  (let [x-mv-fn (case x-dir
                  :right +
                  :left -
                  :none just-a)
        y-mv-fn (case y-dir
                  :up +
                  :down -
                  :none just-a)
        x (x-mv-fn x x-delta)
        y (y-mv-fn y y-delta)
        x-anchored (cond (> x c/game-width-adj) c/game-width-adj
                         (< x 0) 0
                         :else x)
        y-anchored (cond (> y c/game-height-adj) c/game-height-adj
                         (< y 0) 0
                         :else y)
        angle-anchored (x-mv-fn angle c/yaw-change-amt)
        angle-anchored (case x-dir
                         :right (if (> angle-anchored c/yaw-delta-max) c/yaw-delta-max angle-anchored)
                         :left (if (< angle-anchored (- c/yaw-delta-max)) (- c/yaw-delta-max) angle-anchored)
                         :none (angle-reset angle-anchored))]
    (body-position! entity x-anchored y-anchored angle-anchored)
    entity))

(defn move-player-tick [screen entities {:keys [:x :y :angle] :as entity}]
  (let [x-move?  (or (key-pressed? :dpad-right) (key-pressed? :dpad-left))
        y-move?  (or (key-pressed? :dpad-up) (key-pressed? :dpad-down))
        xy-move? (and x-move? y-move?)
        x-delta (* (if x-move? c/gunship-speed 0) (if xy-move? c/gunship-xy-ratio 1.0))
        y-delta (* (if y-move? c/gunship-speed 0) (if xy-move? c/gunship-xy-ratio 1.0))
        x-dir (cond
                (key-pressed? :dpad-right)
                :right
                (key-pressed? :dpad-left)
                :left
                :else :none)
        y-dir (cond
                (key-pressed? :dpad-up)
                :up
                (key-pressed? :dpad-down)
                :down
                :else :none)]
    (if (or x-move? y-move?)
        (move entity x-dir x-delta y-dir y-delta)
        (angle-reset-body entity))))
