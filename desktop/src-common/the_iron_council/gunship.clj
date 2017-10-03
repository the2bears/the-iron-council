(ns the-iron-council.gunship
  (:require [pixel-ships.core :as psc :refer :all]
            [pixel-ships.bollinger :as bollinger :refer :all]
            [the-iron-council.common :as c]
            [play-clj.core :refer [add-timer! bundle shape color key-pressed? pixmap! pixmap* pixmap-format screen! update! x y]]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! vector-2 vector-2!]]))

(def gunship-texture (atom nil))
(def gatling-option-texture (atom nil))
(def rocket-option-texture (atom nil))

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
  ([seed s-model c-scheme]
   (let [ship-map (psc/color-pixel-ship (psc/create-pixel-ship (assoc s-model :seed seed)) c-scheme)
         tags (keys (:pixels ship-map))
         pixels (:pixels ship-map)
         shape-builder (fn[s] (reduce (fn[acc n] (conj acc n)) [] s))]
     (reduce (fn[acc tag](into acc (shape-builder (tag pixels)))) [] tags))))

(defn create-pixel-ship-texture
  ([]
   (create-pixel-ship-texture (rand-int Integer/MAX_VALUE)))
  ([seed]
   (create-pixel-ship-texture seed bollinger/model bollinger/color-scheme))
  ([seed s-model c-scheme]
   (let [pixel-map-list (create-pixel-map-list seed s-model c-scheme)
         pix-map (pixmap* 16 32 (pixmap-format :r-g-b-a8888))]
     (doseq [pixel pixel-map-list] (draw-rect-pixelmap pix-map pixel))
     (assoc (texture pix-map) :seed seed))))

(defn create-option-texture
  [seed s-model c-scheme]
  (let [pixel-map-list (create-pixel-map-list seed s-model c-scheme)
        pix-map (pixmap* 8 16 (pixmap-format :r-g-b-a8888))]
    (doseq [pixel pixel-map-list] (draw-rect-pixelmap pix-map pixel))
    (assoc (texture pix-map) :seed seed)))

(defn create-ship-entity!
  ([{:keys [option-type] :as screen}]
   (let [pixel-ship (-> @gunship-texture
                        (assoc :translate-x (- (c/screen-to-world c/ship-mp-xoffset))
                               :translate-y (- (c/screen-to-world c/ship-mp-yoffset))
                               :width (c/screen-to-world 16)
                               :height (c/screen-to-world 32)))
         option-texture (if (= option-type :rocket) @rocket-option-texture @gatling-option-texture)
         left-option (assoc option-texture :translate-x (- (c/screen-to-world c/ship-option-xoffset-left))
                                           :translate-y (- (c/screen-to-world c/ship-option-yoffset))
                                           :width (c/screen-to-world 8)
                                           :height (c/screen-to-world 16))
         right-option (assoc option-texture :translate-x (+ (c/screen-to-world c/ship-option-xoffset-right))
                                            :translate-y (- (c/screen-to-world c/ship-option-yoffset))
                                            :width (c/screen-to-world 8)
                                            :height (c/screen-to-world 16))
         ship-bundle (bundle pixel-ship left-option right-option)
         x (c/screen-to-world (/ c/game-width 2))
         y c/ship-y-start]

     (assoc ship-bundle
            :id :gunship
            :gunship? true
            :render-layer 90
            :x x
            :y y
            :angle 0
            :collider (circle x y (c/screen-to-world 3))
            :collider-type :circle))))


(defn- angle-reset [angle]
  (let [ccw? (< angle 0)
        yaw-reset-fn (if ccw? + -)
        yaw-reset-amt (if ccw? (if (< (- angle) c/yaw-reset-amt) (- angle) c/yaw-reset-amt)
                          (if (< angle c/yaw-reset-amt) angle c/yaw-reset-amt))]
    (if (c/cannon-key-pressed?) angle (yaw-reset-fn angle yaw-reset-amt))))

(defn- just-a [a b]
  a)

(defn- move [{:keys [x y angle collider] :as entity} x-dir x-delta y-dir y-delta]
  (let [x-mv-fn (case x-dir
                  :right +
                  :left -
                  :none just-a)
        y-mv-fn (case y-dir
                  :up +
                  :down -
                  :none just-a)
        yaw-mv-fn (if c/yaw-with-x
                    (case x-dir
                      :right -
                      :left +
                      :none just-a)
                    (case x-dir
                      :right +
                      :left -
                      :none just-a))
        x (x-mv-fn x x-delta)
        y (y-mv-fn y y-delta)
        x-anchored (cond (> x c/game-width-adj) c/game-width-adj
                         (< x 0) 0
                         :else x)
        y-anchored (cond (> y c/game-height-adj) c/game-height-adj
                         (< y 0) 0
                         :else y)
        angle-anchored (if (c/cannon-key-pressed?) angle (yaw-mv-fn angle c/yaw-change-amt))
        angle-anchored (if c/yaw-with-x
                         (case x-dir
                           :right (if (< angle-anchored (- c/yaw-delta-max)) (- c/yaw-delta-max) angle-anchored)
                           :left (if (> angle-anchored c/yaw-delta-max) c/yaw-delta-max angle-anchored)
                           :none (angle-reset angle-anchored))
                         (case x-dir
                           :right (if (> angle-anchored c/yaw-delta-max) c/yaw-delta-max angle-anchored)
                           :left (if (< angle-anchored (- c/yaw-delta-max)) (- c/yaw-delta-max) angle-anchored)
                           :none (angle-reset angle-anchored)))]
    (circle! collider :set-position x-anchored y-anchored)
    (assoc entity :x x-anchored :y y-anchored :angle angle-anchored)))

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
      (assoc entity :angle (angle-reset angle)))))

(defn create-textures []
  (do
    (reset! gunship-texture (create-pixel-ship-texture Long/MAX_VALUE c/gunship-model c/gunship-color-scheme))
    (reset! gatling-option-texture (create-option-texture Long/MAX_VALUE c/gatling-model c/gunship-color-scheme))
    (reset! rocket-option-texture (create-option-texture Long/MAX_VALUE c/rocket-model c/gunship-color-scheme))))
