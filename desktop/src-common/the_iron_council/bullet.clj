(ns the-iron-council.bullet
  (:require [play-clj.core :refer [shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! fixture! fixture-def polygon-shape]]
            [play-clj.math :refer [vector-2]]
            [the-iron-council.common :as c]))

(def bullet-texture (atom nil))

(defn create-bullet-body!
  [screen x y a]
  (let [body (add-body! screen (body-def :dynamic
                                         :bullet true))
        bullet-vector (vector-2 0 c/bullet-speed :rotate a)]
    (->> (polygon-shape :set-as-box c/bullet-half-width c/bullet-half-height (vector-2 c/bullet-half-width (+ c/bullet-height c/bullet-half-height)) 0)
         (fixture-def :density 0 :friction 0 :restitution 0 :shape)
         (body! body :create-fixture))
    (doto body
      (body-position! (- x c/bullet-half-width) (- y c/bullet-half-height) a)
      (body! :set-linear-velocity (core/x bullet-vector) (core/y bullet-vector)))
    body))

(defn create-bullet!
  [screen x y a]
  (let [bullet (cond (nil? @bullet-texture)
                     (do
                       (reset! bullet-texture (texture "bullet.png" :set-region 0 0 2 4))
                       @bullet-texture)
                     :else @bullet-texture)]
                                        ;(sounds/play-once :bullet)
    (assoc bullet
      :id :bullet
      :bullet? true :render-layer 50
      :body (create-bullet-body! screen x y a)
      :x (- x c/bullet-half-width)
      :y (- y c/bullet-half-height)
      :width c/bullet-width :height (* 2 c/bullet-height))))

(defn handle-collision [bullet other-entity screen entities]
  (cond (:oob? other-entity)
        (do
          ;(prn :handle-collision :oob?)
          (remove #(= bullet %) entities))
        :else entities))
