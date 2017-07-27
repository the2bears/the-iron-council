(ns the-iron-council.bullet
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! fixture! fixture-def polygon-shape]]
            [play-clj.math :refer [vector-2]]
            [the-iron-council.common :as c]
            [the-iron-council.particle :as particle]))

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

(defn create-cannon-shell-body!
  [screen x y a bullet-start-offset-vector]
  (let [body (add-body! screen (body-def :dynamic
                                         :bullet true))
        bullet-vector (vector-2 0 c/bullet-speed :rotate a)]
    (->> (polygon-shape :set-as-box c/bullet-hitbox-side c/bullet-hitbox-side (vector-2 c/bullet-hitbox-x c/bullet-hitbox-y) a)
         (fixture-def :density 0 :friction 0 :restitution 0 :is-sensor true :shape)
         (body! body :create-fixture))
    (doto body
      (body-position!
       (- x (core/x bullet-start-offset-vector))
       (- y (core/y bullet-start-offset-vector))
       a)
      (body! :set-linear-velocity (core/x bullet-vector) (core/y bullet-vector)))
    body))

(defn fire-cannon!
  [screen x y a]
  (let [bullet-start-offset-vector (vector-2 c/bullet-half-width c/bullet-half-height :rotate a)
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
      :body (create-cannon-shell-body! screen x y a bullet-start-offset-vector)
      :x (- x (core/x bullet-start-offset-vector))
      :y (- y (core/y bullet-start-offset-vector))
      :angle a
      :width c/bullet-width :height c/bullet-height)))

(defn- create-gatling-shell-texture []
  (let [pix-map (pixmap* 2 8 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set c/gatling-shell-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (draw-rects pix-map (first color-set) x y w h)))
    (texture pix-map :set-region 0 0 2 8)))

(defn create-gatling-shell-body!
  [screen x y a gatling-start-offset-vector]
  (let [body (add-body! screen (body-def :dynamic
                                         :bullet true))
        bullet-vector (vector-2 0 c/gatling-shell-speed :rotate a)]
    (->> (polygon-shape :set-as-box c/gatling-hitbox-side c/gatling-hitbox-side (vector-2 c/gatling-hitbox-x c/gatling-hitbox-y) a)
         (fixture-def :density 0 :friction 0 :restitution 0 :is-sensor true :shape)
         (body! body :create-fixture))
    (doto body
      (body-position!
       (- x (core/x gatling-start-offset-vector))
       (- y (core/y gatling-start-offset-vector))
       a)
      (body! :set-linear-velocity (core/x bullet-vector) (core/y bullet-vector)))
    body))

(defn fire-gatling!
  [screen x y a]
  (let [gatling-start-offset-vector-left (vector-2 (c/screen-to-world c/gatling-shell-xoffset-left) c/gatling-shell-half-height :rotate a)
        gatling-start-offset-vector-right (vector-2 (c/screen-to-world (- c/gatling-shell-xoffset-right)) c/gatling-shell-half-height :rotate a)
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

        ;shell-casing-left  (particle/create-shell-casing (+ (c/screen-to-world 1) (- x (core/x gatling-start-offset-vector-left))) (- y (core/y gatling-start-offset-vector-left)) a)
        ;shell-casing-right  (particle/create-shell-casing (+ (c/screen-to-world 1) (- x (core/x gatling-start-offset-vector-right))) (- y (core/y gatling-start-offset-vector-right)) a)]
    [;shell-casing-left
     ;shell-casing-right
     (assoc gatling-shell-left
       :id (uuid)
       :bullet? true
       :render-layer 50
       :ttl 100
       :body (create-gatling-shell-body! screen x y a gatling-start-offset-vector-left)
       :x (- x (core/x gatling-start-offset-vector-left))
       :y (- y (core/y gatling-start-offset-vector-left))
       :angle a
       :width c/gatling-shell-width :height c/gatling-shell-height)
     (assoc gatling-shell-right
       :id (uuid)
       :bullet? true
       :render-layer 50
       :ttl 100
       :body (create-gatling-shell-body! screen x y a gatling-start-offset-vector-right)
       :x (- x (core/x gatling-start-offset-vector-right))
       :y (- y (core/y gatling-start-offset-vector-right))
       :angle a
       :width c/gatling-shell-width :height c/gatling-shell-height)]))

(defn- create-rocket-texture []
  (let [pix-map (pixmap* 4 4 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set c/rocket-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (draw-rects pix-map (first color-set) x y w h)))
    (texture pix-map :set-region 0 0 3 4)))

(defn create-rocket-body!
  [screen x y a rocket-start-offset-vector]
  (let [body (add-body! screen (body-def :dynamic
                                         :bullet true))
        bullet-vector (vector-2 0 c/rocket-speed :rotate a)]
    (->> (polygon-shape :set-as-box c/rocket-hitbox-side c/rocket-hitbox-side (vector-2 c/rocket-hitbox-x c/rocket-hitbox-y) a)
         (fixture-def :density 0 :friction 0 :restitution 0 :is-sensor true :shape)
         (body! body :create-fixture))
    (doto body
      (body-position!
       (- x (core/x rocket-start-offset-vector))
       (- y (core/y rocket-start-offset-vector))
       a)
      (body! :set-linear-velocity (core/x bullet-vector) (core/y bullet-vector)))
    body))

(defn fire-rocket!
  [screen x y a]
  (let [rocket-start-offset-vector-left (vector-2 (c/screen-to-world c/rocket-xoffset-left) c/rocket-half-height :rotate a)
        rocket-start-offset-vector-right (vector-2 (c/screen-to-world (- c/rocket-xoffset-right)) c/rocket-half-height :rotate a)
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
       :body (create-rocket-body! screen x y a rocket-start-offset-vector-left)
       :x (- x (core/x rocket-start-offset-vector-left))
       :y (- y (core/y rocket-start-offset-vector-left))
       :angle a
       :width c/rocket-width :height c/rocket-height)
     (assoc rocket-right
       :id (uuid)
       :bullet? true
       :render-layer 50
       :ttl 200
       :body (create-rocket-body! screen x y a rocket-start-offset-vector-right)
       :x (- x (core/x rocket-start-offset-vector-right))
       :y (- y (core/y rocket-start-offset-vector-right))
       :angle a
       :width c/rocket-width :height c/rocket-height)]))


(defn handle-collision [bullet other-entity screen entities]
  (prn :bullet-handle-collision)
  (cond ;(:oob? other-entity)
        ;(remove #(= (:id bullet) (:id %)) entities)
        :else entities))

(defn handle-bullet [screen {:keys [ttl] :as bullet}]
  (when (> ttl 0)
    (assoc bullet :ttl (dec ttl))))
