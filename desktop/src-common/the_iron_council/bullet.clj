(ns the-iron-council.bullet
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! polygon polygon! vector-2]]
            [the-iron-council.common :as c]
            [the-iron-council.utils :as utils]))

(def cannon-shell-texture (atom nil))
(def gatling-shell-texture (atom nil))
(def rocket-texture (atom nil))

(defn- create-cannon-shell-texture []
  (let [pix-map (pixmap* 8 16 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set c/bullet-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (utils/pix-map-rect pix-map (first color-set) x y w h)))
    (texture pix-map :set-region 0 0 6 14)))

(defn fire-cannon!
  [screen x y a]
  (let [bullet-start-offset-vector (vector-2 c/bullet-half-width c/bullet-half-height :rotate a)
        cannon-velocity-vector (vector-2 0 c/bullet-speed :rotate a)
        x (- x (core/x bullet-start-offset-vector))
        y (- y (core/y bullet-start-offset-vector))
        c-p-verts (float-array [x (+ y (- c/bullet-height c/bullet-width))
                                (+ x c/bullet-width) (+ y (- c/bullet-height c/bullet-width))
                                (+ x c/bullet-width) (+ y c/bullet-height c/bullet-speed)
                                x (+ y c/bullet-height c/bullet-speed)])
        collider-poly (polygon c-p-verts)]
    (doto collider-poly
      (polygon! :set-origin x y)
      (polygon! :set-rotation a))
    (assoc @cannon-shell-texture
      :id (c/uuid)
      :bullet? true
      :render-layer 50
      :ttl 120
      :x x
      :y y
      :angle a
      :velocity cannon-velocity-vector
      :collider (polygon (polygon! collider-poly :get-transformed-vertices))
      :collider-type :poly
      :c-x-offset (c/screen-to-world 3)
      :c-y-offset (c/screen-to-world 11)
      :width c/bullet-width
      :height c/bullet-height)))

(defn gatling-collider-verts [x y]
  (float-array [x (+ y (c/screen-to-world 6))
                (+ x (c/screen-to-world 2)) (+ y (c/screen-to-world 6))
                (+ x (c/screen-to-world 2)) (+ y (c/screen-to-world 8) c/gatling-shell-speed)
                x (+ y (c/screen-to-world 8) c/gatling-shell-speed)]))

(defn- create-gatling-shell-texture []
  (let [pix-map (pixmap* 2 8 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set c/gatling-shell-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (utils/pix-map-rect pix-map (first color-set) x y w h)))
    (texture pix-map :set-region 0 0 2 8)))

(defn fire-gatling!
  [screen x y a]
  (let [gatling-start-offset-vector-left (vector-2 (c/screen-to-world c/gatling-shell-xoffset-left) c/gatling-shell-half-height :rotate a)
        gatling-start-offset-vector-right (vector-2 (c/screen-to-world (- c/gatling-shell-xoffset-right)) c/gatling-shell-half-height :rotate a)
        gatling-velocity-vector (vector-2 0 c/gatling-shell-speed :rotate a)
        x-l (- x (core/x gatling-start-offset-vector-left))
        y-l (- y (core/y gatling-start-offset-vector-left))
        x-r (- x (core/x gatling-start-offset-vector-right))
        y-r (- y (core/y gatling-start-offset-vector-right))
        collider-left-poly (polygon (gatling-collider-verts x-l y-l))
        collider-right-poly (polygon (gatling-collider-verts x-r y-r))]
    (doto collider-left-poly
      (polygon! :set-origin x-l y-l)
      (polygon! :set-rotation a))
    (doto collider-right-poly
      (polygon! :set-origin x-r y-r)
      (polygon! :set-rotation a))
    [(assoc @gatling-shell-texture
       :id (c/uuid)
       :bullet? true
       :render-layer 50
       :ttl 100
       :x x-l
       :y y-l
       :angle a
       :velocity gatling-velocity-vector
       :collider (polygon (polygon! collider-left-poly :get-transformed-vertices))
       :collider-type :poly
       :c-x-offset (c/screen-to-world 1)
       :c-y-offset (c/screen-to-world 6)
       :width c/gatling-shell-width
       :height c/gatling-shell-height)
     (assoc @gatling-shell-texture
       :id (c/uuid)
       :bullet? true
       :render-layer 50
       :ttl 100
       :x x-r
       :y y-r
       :angle a
       :velocity gatling-velocity-vector
       :collider (polygon (polygon! collider-right-poly :get-transformed-vertices))
       :collider-type :poly
       :c-x-offset (c/screen-to-world 1)
       :c-y-offset (c/screen-to-world 6)
       :width c/gatling-shell-width
       :height c/gatling-shell-height)]))

(defn rocket-collider-verts [x y]
  (float-array [x (+ y (c/screen-to-world 2))
                (+ x (c/screen-to-world 3)) (+ y (c/screen-to-world 2))
                (+ x (c/screen-to-world 3)) (+ y (c/screen-to-world 4) c/rocket-speed)
                x (+ y (c/screen-to-world 4) c/rocket-speed)]))

(defn- create-rocket-texture []
  (let [pix-map (pixmap* 4 4 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set c/rocket-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (utils/pix-map-rect pix-map (first color-set) x y w h)))
    (texture pix-map :set-region 0 0 3 4)))

(defn fire-rocket!
  [screen x y a]
  (let [rocket-start-offset-vector-left (vector-2 (c/screen-to-world c/rocket-xoffset-left) c/rocket-half-height :rotate a)
        rocket-start-offset-vector-right (vector-2 (c/screen-to-world (- c/rocket-xoffset-right)) c/rocket-half-height :rotate a)
        rocket-velocity-vector (vector-2 0 c/rocket-speed :rotate a)
        x-l (- x (core/x rocket-start-offset-vector-left))
        y-l (- y (core/y rocket-start-offset-vector-left))
        x-r (- x (core/x rocket-start-offset-vector-right))
        y-r (- y (core/y rocket-start-offset-vector-right))
                                        ;(sounds/play-once :bullet)
        collider-left-poly (polygon (rocket-collider-verts x-l y-l))
        collider-right-poly (polygon (rocket-collider-verts x-r y-r))]
    (doto collider-left-poly
      (polygon! :set-origin x-l y-l)
      (polygon! :set-rotation a))
    (doto collider-right-poly
      (polygon! :set-origin x-r y-r)
      (polygon! :set-rotation a))
    [(assoc @rocket-texture
       :id (c/uuid)
       :bullet? true
       :render-layer 50
       :ttl 200
       :x x-l
       :y y-l
       :angle a
       :velocity rocket-velocity-vector
       :collider (polygon (polygon! collider-left-poly :get-transformed-vertices))
       :collider-type :poly
       :c-x-offset (c/screen-to-world 1.5)
       :c-y-offset (c/screen-to-world 3)
       :width c/rocket-width
       :height c/rocket-height)
     (assoc @rocket-texture
       :id (c/uuid)
       :bullet? true
       :render-layer 50
       :ttl 200
       :x x-r
       :y y-r
       :angle a
       :velocity rocket-velocity-vector
       :collider (polygon (polygon! collider-right-poly :get-transformed-vertices))
       :collider-type :poly
       :c-x-offset (c/screen-to-world 1.5)
       :c-y-offset (c/screen-to-world 3)
       :width c/rocket-width
       :height c/rocket-height)]))

(defn create-textures []
  (do
    (reset! cannon-shell-texture (create-cannon-shell-texture))
    (reset! rocket-texture (create-rocket-texture))
    (reset! gatling-shell-texture (create-gatling-shell-texture))))

(defn handle-collision [bullet other-entity screen entities]
  (cond ;(:oob? other-entity)
        ;(remove #(= (:id bullet) (:id %)) entities)
        :else entities))

(defn move-bullet [screen {:keys [ttl x y c-x-offset c-y-offset velocity collider] :as bullet}]
  (let [dx (core/x velocity)
        dy (core/y velocity)]
    (when (> ttl 0)
      (do
        (polygon! collider :translate dx dy)
        (assoc bullet :ttl (dec ttl) :x (+ x dx) :y (+ y dy) :collider (polygon (polygon! collider :get-transformed-vertices)))))))
