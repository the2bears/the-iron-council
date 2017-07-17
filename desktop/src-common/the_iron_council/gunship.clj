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
   (let [ship-map (psc/color-pixel-ship (psc/create-pixel-ship (assoc bollinger/model :seed seed)) c-model)
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
         pix-map (pixmap* 16 16 (pixmap-format :r-g-b-a8888))]
     ;(pixmap! pix-map :set-color (color :white))
     ;(pixmap! pix-map :fill-rectangle 0 0 16 16)
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
   (let [pixel-ship (create-pixel-ship-texture Integer/MAX_VALUE c/gunship-color-scheme)]
     (doto (assoc pixel-ship
             :body (create-ship-body! screen)
             :width (c/screen-to-world 16) :height (c/screen-to-world 16)
             :id :gunship
             :gunship? true
             :render-layer 90
             :translate-x (- (c/screen-to-world c/ship-mp-xoffset))
             :translate-y (- (c/screen-to-world c/ship-mp-yoffset)))
       (body-position! (c/screen-to-world (/ c/game-width 2)) c/ship-y-start 0)
       (body! :set-linear-velocity 0 0)))))

(defn- move [screen entities {:keys [:x :y :angle] :as entity} direction]
  (let [mv-fn (case direction
                :right +
                :left -)
        x (mv-fn x c/gunship-speed)
        x-anchored (cond (> x c/game-width-adj) c/game-width-adj
                         (< x 0) 0
                         :else x)
        y-anchored (cond (> y c/game-height-adj) c/game-height-adj
                         (< y 0) 0
                         :else y)]
    (body-position! entity x-anchored y-anchored (mv-fn angle c/yaw-change-amt)))
  entity)

(defn- angle-reset [{:keys [:x :y :angle] :as entity}]
  (let [ccw? (< angle 0)
        yaw-reset-fn (if ccw? + -)
        yaw-reset-amt (if ccw? (if (< (- angle) c/yaw-reset-amt) (- angle) c/yaw-reset-amt)
                               (if (< angle c/yaw-reset-amt) angle c/yaw-reset-amt))]
    (body-position! entity x y (yaw-reset-fn angle yaw-reset-amt))))

(defn move-player-tick [screen entities {:keys [:x :y :angle] :as entity}]
   (cond
     (key-pressed? :dpad-right)
     (move screen entities entity :right)
     (key-pressed? :dpad-left)
     (move screen entities entity :left)
     :else (angle-reset entity))
   entity)
