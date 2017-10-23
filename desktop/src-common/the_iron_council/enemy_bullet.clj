(ns the-iron-council.enemy-bullet
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! polygon polygon! vector-2 vector-2!]]
            [the-iron-council.bullet-hell :as bh]
            [the-iron-council.common :as c]
            [the-iron-council.utils :as utils]))


(def med-bullet-texture (atom nil))
(def long-bullet-texture (atom nil))
(def med-bullet-rects [[(color 1.0 0 1.0 1) [2 0 4 8 1 1 6 6 0 2 8 4]]
                       [(color 1.0 0.5 1.0 1) [2 1 4 6 1 2 6 4]]
                       [(color :white) [3 1 2 6 2 2 4 4 1 3 6 2]]])
(def long-bullet-rects [[(color 1.0 0 1.0 1) [8 2 4 6 9 0 2 10]]
                        [(color 1.0 0.5 1.0 1) [9 1 2 8]]
                        [(color :white) [9 2 2 6]]])




(def bullet-speed (c/screen-to-world 0.2))
(def bullet-speed2 (c/screen-to-world 1))

(def upper-edge (+ c/game-height-adj c/oob-padding))
(def lower-edge (- c/oob-padding))
(def right-edge (+ c/game-width-adj c/oob-padding))
(def left-edge (- c/oob-padding))

(defn- in-bounds [bullet]
  (if-let [{:keys [x y]} bullet]
    (and (< left-edge x right-edge)
         (< lower-edge y upper-edge))))

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

(defn create-bullet-textures! []
  (let [pix-map (pixmap* 16 16 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set med-bullet-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (utils/pix-map-rect pix-map (first color-set) x y w h)))
    (doseq [color-set long-bullet-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (utils/pix-map-rect pix-map (first color-set) x y w h)))
    (reset! med-bullet-texture (-> (texture pix-map :set-region 0 0 8 8)
                                   (assoc :enemy-bullet? true
                                          :render-layer 60
                                          :width (c/screen-to-world 8)
                                          :height (c/screen-to-world 8)
                                          :translate-x (- (c/screen-to-world 4))
                                          :translate-y (- (c/screen-to-world 4)))))
    (reset! long-bullet-texture (-> (texture pix-map :set-region 8 0 4 10)
                                    (assoc :enemy-bullet? true
                                           :render-layer 60
                                           :width (c/screen-to-world 4)
                                           :height (c/screen-to-world 10)
                                           :translate-x (- (c/screen-to-world 2))
                                           :translate-y (- (c/screen-to-world 5)))))))
(defn init! []
  (create-bullet-textures!))

(defn fire-turret-bullet [screen x y a]
  (let [bullet-velocity-vector (vector-2 0 (* bullet-speed2 2) :rotate a)
        constant-velocity (bh/linear-movement
                           :dx (core/x bullet-velocity-vector)
                           :dy (core/y bullet-velocity-vector)
                           :max-ticks 45)
        change-speed (bh/change-speed
                      :sx (core/x bullet-velocity-vector)
                      :sy (core/y bullet-velocity-vector)
                      :tx (/ (core/x bullet-velocity-vector) 3)
                      :ty (/ (core/y bullet-velocity-vector) 3)
                      :max-ticks 60)
        split (bh/split
               :dx (core/x bullet-velocity-vector)
               :dy (core/y bullet-velocity-vector)
               :da 10
               :max-ticks 62)
        continue (bh/continue :max-ticks 600)
        constant-velocity-2 (bh/linear-movement
                             :dx (core/x bullet-velocity-vector)
                             :dy (core/y bullet-velocity-vector)
                             :max-ticks 61)
        constant-velocity-3 (bh/linear-movement
                             :dx (core/x bullet-velocity-vector)
                             :dy (core/y bullet-velocity-vector))]
                             ;:max-ticks 90)]
    (assoc @long-bullet-texture
           :id (c/uuid)
           :x x
           :y y
           :angle a
           :collider {:x x :y y :r (c/screen-to-world 2)}
           :collider-type :circle
           :bullet-hell-fn (some-fn ;change-speed constant-velocity-2 split continue;constant-velocity-2)))) ;continue))))
                             constant-velocity-3)))) ; split constant-velocity-2)))); continue))))

(defn test-bullet!
  [screen x y a]
  (let [bullet-velocity-vector (vector-2 0 bullet-speed :rotate a)
        bullet-velocity-vector2 (vector-2 0 bullet-speed2 :rotate a)
        change-speed0 (bh/change-speed
                       :tx 0 ;(core/x bullet-velocity-vector2)
                       :sy bullet-speed ;(core/y bullet-velocity-vector2)
                       :ty 0
                       :min-ticks 0
                       :max-ticks 240)
        wait (bh/continue :max-ticks 300)
        change-speed (bh/change-speed
                      :tx 0 ;(core/x bullet-velocity-vector2)
                      :ty (* 4 bullet-speed2) ;(core/y bullet-velocity-vector2)
                      :min-ticks 300
                      :max-ticks 360)
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
               :ta 135
               :min-ticks 120
               :max-ticks 360)
        wait2 (bh/continue :max-ticks 1720)
        linear3 (bh/linear-movement
                 :dx 0
                 :dy 0
                 :max-ticks 120)
        linear4 (bh/linear-movement
                 :dx 0.0
                 :dy bullet-speed
                 :max-ticks 121)
        rotate1 (bh/rotate
                 :da -0.15
                 :min-ticks 121
                 :max-ticks 3600)]
    (assoc @med-bullet-texture
           :id (c/uuid)
           :render-layer 60
           :x (c/screen-to-world x)
           :y (c/screen-to-world y)
           :angle a
           :collider {:x (c/screen-to-world x) :y (c/screen-to-world y) :r (c/screen-to-world 3)}
           :collider-type :circle
           :bullet-hell-fn ;(some-fn linear3 linear4 rotate1 wait2))))
              ;(some-fn linear1 turn1 wait2))))
              (some-fn change-speed0 wait change-speed wait2))))

(defn handle-bullet [screen {:keys [bullet-hell-fn] :as entity}]
  (let [bullets (flatten (conj [] (bullet-hell-fn entity)))]
    (loop [bullets bullets
           acc []]
      (if (seq bullets)
        (let [{:keys [x y collider] :as bullet} (first bullets)]
          (when (in-bounds bullet)
                (recur (rest bullets) (conj acc (assoc bullet :collider (assoc collider :x x :y y))))))
        acc))))

(defn start-rapid-fire [screen entities {:keys [id] :as source}]
  {:bullet-hell? true
   :id (c/uuid)
   :ticks 0
   :ticks-between-shots 5
   :shots 0
   :total-shots 4
   :source-id id
   :target-id :gunship})

(defn handle-bullet-hell [screen entities {:keys [id ticks ticks-between-shots shots total-shots source-id target-id] :as entity}]
  (if-let [{:keys [x y angle] :as source-entity} (first (filter #(= source-id (:id %)) entities))]
    (let [bullet-velocity-vector2 (vector-2 0 (* bullet-speed2 2) :rotate angle);(mod angle 360))
          constant-velocity (bh/linear-movement
                             :dx (core/x bullet-velocity-vector2)
                             :dy (core/y bullet-velocity-vector2))
          fire? (= 0 (mod ticks ticks-between-shots))]
      [(when fire?
          (assoc @long-bullet-texture
                   :id (c/uuid)
                   :x x
                   :y y
                   :angle angle; (mod angle 360)
                   :collider {:x x :y y :r (c/screen-to-world 2)}
                   :collider-type :circle
                   :bullet-hell-fn (some-fn constant-velocity)))
       (when (< shots total-shots)
         (assoc entity :ticks (inc ticks)
                       :shots (if fire? (inc shots) shots)))])))
