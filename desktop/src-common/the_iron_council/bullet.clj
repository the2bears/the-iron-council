(ns the-iron-council.bullet
  (:require [play-clj.core :refer [shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! fixture! fixture-def polygon-shape]]
            [play-clj.math :refer [vector-2]]
            [the-iron-council.common :as c]))

(def bullet-texture (atom nil))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn create-bullet-body!
  [screen x y a]
  (let [body (add-body! screen (body-def :dynamic
                                         :bullet true))
        bullet-vector (vector-2 0 c/bullet-speed :rotate a)
        bullet-start-offset-vector (vector-2 c/create-bullet-x-offset c/create-bullet-y-offset :rotate a)]
    (->> (polygon-shape :set-as-box c/bullet-half-width c/bullet-half-height (vector-2 c/bullet-half-width (+ c/bullet-height c/bullet-half-height)) a)
         (fixture-def :density 0 :friction 0 :restitution 0 :is-sensor true :shape)
         (body! body :create-fixture))
    (doto body
      (body-position!
       (+ (- x c/bullet-half-width) (core/x bullet-start-offset-vector));(- x c/bullet-half-width)
       (+ (- y c/bullet-half-height) (core/y bullet-start-offset-vector));(- y c/bullet-half-height)
       a)
      (body! :set-linear-velocity (core/x bullet-vector) (core/y bullet-vector)))
    body))

(defn create-bullet!
  [screen x y a]
  (let [bullet-start-offset-vector (vector-2 c/create-bullet-x-offset c/create-bullet-y-offset :rotate a)
        bullet (cond (nil? @bullet-texture)
                     (do
                       (reset! bullet-texture (texture "bullet.png" :set-region 0 0 2 4))
                       @bullet-texture)
                     :else @bullet-texture)]
                                        ;(sounds/play-once :bullet)
    (assoc bullet
      :id (uuid)
      :bullet? true
      :render-layer 50
      :ttl 120
      :body (create-bullet-body! screen x y a); (+ x (core/x bullet-start-offset-vector)) (+ y (core/y bullet-start-offset-vector)) a
      :x (+ (- x c/bullet-half-width) (core/x bullet-start-offset-vector))
      :y (+ (- y c/bullet-half-height) (core/y bullet-start-offset-vector))
      :angle a
      :width c/bullet-width :height (* 2 c/bullet-height))))

(defn handle-collision [bullet other-entity screen entities]
  (cond ;(:oob? other-entity)
        ;(remove #(= (:id bullet) (:id %)) entities)
        :else entities))

(defn handle-bullet [screen {:keys [ttl] :as bullet}]
  (when (> ttl 0)
    (assoc bullet :ttl (dec ttl))))
