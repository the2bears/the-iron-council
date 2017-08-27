(ns the-iron-council.bullet
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [vector-2]]
            [the-iron-council.common :as c]))

(def cannon-shell-texture (atom nil))
(def gatling-shell-texture (atom nil))
(def rocket-texture (atom nil))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn- draw-rects [pix-map c x y w h]
  (doto pix-map
    (pixmap! :set-color c)
    (pixmap! :fill-rectangle x y w h)))

(defn- create-cannon-shell-texture []
  (let [pix-map (pixmap* 8 16 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set c/bullet-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (draw-rects pix-map (first color-set) x y w h)))
    (texture pix-map :set-region 0 0 6 14)))

(defn fire-cannon!
  [screen x y a]
  (let [bullet-start-offset-vector (vector-2 c/bullet-half-width c/bullet-half-height :rotate a)
        cannon-velocity-vector (vector-2 0 c/bullet-speed :rotate a)
        cannon-shell (cond (nil? @cannon-shell-texture)
                       (do
                         (reset! cannon-shell-texture (create-cannon-shell-texture))
                         @cannon-shell-texture)
                      :else @cannon-shell-texture)]
                                        ;(sounds/play-once :bullet)
    (assoc cannon-shell
      :id (uuid)
      :bullet? true
      :render-layer 50
      :ttl 120
      :x (- x (core/x bullet-start-offset-vector))
      :y (- y (core/y bullet-start-offset-vector))
      :angle a
      :velocity cannon-velocity-vector
      :width c/bullet-width :height c/bullet-height)))

(defn- create-gatling-shell-texture []
  (let [pix-map (pixmap* 2 8 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set c/gatling-shell-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (draw-rects pix-map (first color-set) x y w h)))
    (texture pix-map :set-region 0 0 2 8)))

(defn fire-gatling!
  [screen x y a]
  (let [gatling-start-offset-vector-left (vector-2 (c/screen-to-world c/gatling-shell-xoffset-left) c/gatling-shell-half-height :rotate a)
        gatling-start-offset-vector-right (vector-2 (c/screen-to-world (- c/gatling-shell-xoffset-right)) c/gatling-shell-half-height :rotate a)
        gatling-velocity-vector (vector-2 0 c/gatling-shell-speed :rotate a)
        gatling-shell-left (cond (nil? @gatling-shell-texture)
                             (do
                               (reset! gatling-shell-texture (create-gatling-shell-texture))
                               @gatling-shell-texture)
                             :else @gatling-shell-texture)
        gatling-shell-right (cond (nil? @gatling-shell-texture)
                              (do
                                (reset! gatling-shell-texture (create-gatling-shell-texture))
                                @gatling-shell-texture)
                              :else @gatling-shell-texture)]
                                        ;(sounds/play-once :bullet)
    [(assoc gatling-shell-left
       :id (uuid)
       :bullet? true
       :render-layer 50
       :ttl 100
       :x (- x (core/x gatling-start-offset-vector-left))
       :y (- y (core/y gatling-start-offset-vector-left))
       :angle a
       :velocity gatling-velocity-vector
       :width c/gatling-shell-width :height c/gatling-shell-height)
     (assoc gatling-shell-right
       :id (uuid)
       :bullet? true
       :render-layer 50
       :ttl 100
       :x (- x (core/x gatling-start-offset-vector-right))
       :y (- y (core/y gatling-start-offset-vector-right))
       :angle a
       :velocity gatling-velocity-vector
       :width c/gatling-shell-width :height c/gatling-shell-height)]))

(defn- create-rocket-texture []
  (let [pix-map (pixmap* 4 4 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set c/rocket-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (draw-rects pix-map (first color-set) x y w h)))
    (texture pix-map :set-region 0 0 3 4)))

(defn fire-rocket!
  [screen x y a]
  (let [rocket-start-offset-vector-left (vector-2 (c/screen-to-world c/rocket-xoffset-left) c/rocket-half-height :rotate a)
        rocket-start-offset-vector-right (vector-2 (c/screen-to-world (- c/rocket-xoffset-right)) c/rocket-half-height :rotate a)
        rocket-velocity-vector (vector-2 0 c/rocket-speed :rotate a)
        rocket-left (cond (nil? @rocket-texture)
                      (do
                        (reset! rocket-texture (create-rocket-texture))
                        @rocket-texture)
                      :else @rocket-texture)
        rocket-right (cond (nil? @rocket-texture)
                       (do
                         (reset! rocket-texture (create-rocket-texture))
                         @rocket-texture)
                       :else @rocket-texture)]
                                        ;(sounds/play-once :bullet)
    [(assoc rocket-left
       :id (uuid)
       :bullet? true
       :render-layer 50
       :ttl 200
       ;:body (create-rocket-body! screen x y a rocket-start-offset-vector-left)
       :x (- x (core/x rocket-start-offset-vector-left))
       :y (- y (core/y rocket-start-offset-vector-left))
       :angle a
       :velocity rocket-velocity-vector
       :width c/rocket-width :height c/rocket-height)
     (assoc rocket-right
       :id (uuid)
       :bullet? true
       :render-layer 50
       :ttl 200
       ;:body (create-rocket-body! screen x y a rocket-start-offset-vector-right)
       :x (- x (core/x rocket-start-offset-vector-right))
       :y (- y (core/y rocket-start-offset-vector-right))
       :angle a
       :velocity rocket-velocity-vector
       :width c/rocket-width :height c/rocket-height)]))


(defn handle-collision [bullet other-entity screen entities]
  (cond ;(:oob? other-entity)
        ;(remove #(= (:id bullet) (:id %)) entities)
        :else entities))

(defn move-bullet [screen {:keys [ttl x y velocity] :as bullet}]
  (let [dx (core/x velocity)
        dy (core/y velocity)]
    (when (> ttl 0)
      (assoc bullet :ttl (dec ttl) :x (+ x dx) :y (+ y dy)))))
