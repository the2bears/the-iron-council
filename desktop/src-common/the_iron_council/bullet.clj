(ns the-iron-council.bullet
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! fixture! fixture-def polygon-shape]]
            [play-clj.math :refer [vector-2]]
            [the-iron-council.common :as c]))

(def bullet-texture (atom nil))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn- draw-rects [pix-map c x y w h]
  (doto pix-map
    (pixmap! :set-color c)
    (pixmap! :fill-rectangle x y w h)))

(defn- create-bullet-texture []
  (let [pix-map (pixmap* 8 16 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set c/bullet-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (draw-rects pix-map (first color-set) x y w h)))
    (texture pix-map :set-region 0 0 6 14)))

(defn create-bullet-body!
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

(defn create-bullet!
  [screen x y a]
  (let [bullet-start-offset-vector (vector-2 c/bullet-half-width c/bullet-half-height :rotate a)
        bullet (cond (nil? @bullet-texture)
                     (do
                       (reset! bullet-texture (create-bullet-texture))
                       @bullet-texture)
                     :else @bullet-texture)]
                                        ;(sounds/play-once :bullet)
    (assoc bullet
      :id (uuid)
      :bullet? true
      :render-layer 50
      :ttl 120
      :body (create-bullet-body! screen x y a bullet-start-offset-vector)
      :x (- x (core/x bullet-start-offset-vector))
      :y (- y (core/y bullet-start-offset-vector))
      :angle a
      :width c/bullet-width :height c/bullet-height)))

(defn handle-collision [bullet other-entity screen entities]
  (cond ;(:oob? other-entity)
        ;(remove #(= (:id bullet) (:id %)) entities)
        :else entities))

(defn handle-bullet [screen {:keys [ttl] :as bullet}]
  (when (> ttl 0)
    (assoc bullet :ttl (dec ttl))))
