(ns the-iron-council.enemy-bullet
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! polygon polygon! vector-2]]
            [the-iron-council.bullet-hell :as bh]
            [the-iron-council.common :as c]
            [the-iron-council.utils :as utils]))


(def bullet-texture (atom nil))
(def bullet-rects [[(color 1.0 0 1.0 1) [2 0 4 8 1 1 6 6 0 2 8 4]]
                   [(color 1.0 0.5 1.0 1) [2 1 4 6 1 2 6 4]]
                   [(color :white) [3 1 2 6 2 2 4 4 1 3 6 2]]])
(def bullet-speed (c/screen-to-world 0.4))
(def bullet-speed2 (c/screen-to-world 1))

(defn- simple-movement
  ([velocity min-ticks max-ticks]
   (simple-movement (core/x velocity) (core/y velocity) min-ticks max-ticks))
  ([dx dy min-ticks max-ticks]
   (fn [{:keys [x y ticks] :or {ticks 0} :as bullet}]
     (when (<= min-ticks ticks max-ticks)
       (assoc bullet
              :x (+ x dx)
              :y (+ y dy)
              :ticks (inc ticks))))))

(defn- create-bullet-texture []
  (let [pix-map (pixmap* 8 8 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set bullet-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (utils/pix-map-rect pix-map (first color-set) x y w h)))
    (texture pix-map :set-region 0 0 8 8)))

(defn test-bullet!
  [screen x y a]
  (let [bullet-velocity-vector (vector-2 0 bullet-speed :rotate a)
        bullet-velocity-vector2 (vector-2 0 bullet-speed2 :rotate a)
        bullet (cond (nil? @bullet-texture)
                     (do
                       (reset! bullet-texture (create-bullet-texture))
                       @bullet-texture)
                     :else @bullet-texture)
        change-speed0 (bh/change-speed
                       :tx 0 ;(core/x bullet-velocity-vector2)
                       :sy bullet-speed ;(core/y bullet-velocity-vector2)
                       :ty 0
                       :min-ticks 0
                       :max-ticks 240)
        wait (bh/continue :max-ticks 300)
        change-speed (bh/change-speed
                      :tx 0 ;(core/x bullet-velocity-vector2)
                      :ty bullet-speed2 ;(core/y bullet-velocity-vector2)
                      :min-ticks 300
                      :max-ticks 420)
        bhf-2 (bh/linear-movement
               :dx (core/x bullet-velocity-vector2)
               :dy (core/y bullet-velocity-vector2)
               :min-ticks 420)
        linear1 (bh/linear-movement
                 :dy bullet-speed
                 :max-ticks 120)
        turn1 (bh/change-direction
               :sx 0
               :sy bullet-speed
               :ta 45
               :min-ticks 120
               :max-ticks 360)
        wait2 (bh/continue :max-ticks 720)
        linear3 (bh/linear-movement
                 :dx 0
                 :dy 0
                 :max-ticks 120)
        accel1 (bh/accel
                :ax 0.0
                :ay 0.005
                :min-ticks 120
                :max-ticks 780)]
    (assoc bullet
           :enemy-bullet? true
           :render-layer 60
           :x (c/screen-to-world x)
           :y (c/screen-to-world y)
           :angle a
           :width (c/screen-to-world 8)
           :height (c/screen-to-world 8)
           :translate-x (- (c/screen-to-world 4))
           :translate-y (- (c/screen-to-world 4))
           :bullet-hell-fn (some-fn linear3 accel1)))) ;linear1 turn1 wait2)))) ; change-speed0 wait change-speed bhf-2))))

(defn handle-bullet [screen {:keys [bullet-hell-fn] :as entity}]
  (let [move-fn bullet-hell-fn]
    (move-fn entity)))
