(ns the-iron-council.enemy
  (:require [play-clj.core :refer [bundle color pixmap! pixmap* pixmap-format shape update! x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! polygon polygon! rectangle rectangle! vector-2 vector-2!]]
            [the-iron-council.common :as c]
            [the-iron-council.enemy-bullet :as eb]
            [the-iron-council.utils :as utils]))

(def rec-side (c/screen-to-world 10))
(def rec-offset (- (/ rec-side 2)))

(def train-car-width 36)
(def train-car-length 72)
(def train-car-width-adj (c/screen-to-world train-car-width))
(def train-car-length-adj (c/screen-to-world train-car-length))
(def train-car-width-offset (/ train-car-width-adj 2))
(def train-car-length-offset (/ train-car-length-adj 2))
(def indestructible-collider-width (c/screen-to-world 4))
(def indestructible-collider-length train-car-length-adj)

(def cannon-side (c/screen-to-world 16))

(def train-car-entity (atom nil))
(def cannon-entity (atom nil))
(def darkest (color :black))
(def dark (color 0 29/255 71/255 1))
(def dark-highlight (color 55/255 75/255 91/255 1))

(def train-rects [[darkest [0 0 train-car-width train-car-length]]
                  ;[dark-highlight [1 1 34 70]]
                  ;[dark [2 2 32 68]]
                  [dark [1 1 34 70]]])

(def cannon-rects [[(color :blue) [0 4 12 12]]
                   [(color :white) [4 0 4 10]]])

(defn make-targeting-rotation-fn [target-id angle-delta]
  (fn [entities _ {:keys [x y angle] :as entity}]
    (let [target (first (filter #(= target-id (:id %)) entities))
          t-x (or (:x target) x)
          t-y (or (:y target) y)
          angle-to-target (vector-2! (vector-2! (vector-2 t-x t-y) :sub (vector-2 x y)) :angle)
          angle-to-target (if target (mod (- angle-to-target 90) 360) 180)
          angle (+ angle 360)
          angle-diff (mod (- angle-to-target angle) 360)
          angle-delta-fn (cond (> angle-to-target angle)
                               (if (< angle-diff 180) - +)
                               :else
                               (if (< angle-diff 180) + -))
          new-a (if (< angle-diff angle-delta) angle-to-target (angle-delta-fn angle angle-delta))]
      (assoc entity :angle new-a))))

(def cannon-target-fn (make-targeting-rotation-fn :gunship 0.8))

(defn parent-angle-fn [_ parent entity]
  (assoc entity :angle (:angle parent)))

(defn- create-cannon-entity []
  (let [pix-map (pixmap* 16 16 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set cannon-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (utils/pix-map-rect pix-map (first color-set) x y w h)))
    (reset! cannon-entity (assoc (texture pix-map :set-region 0 0 12 16)
                                 :render-layer 6
                                 :width (c/screen-to-world 12)
                                 :height (c/screen-to-world 16)
                                 :armament? true
                                 :enemy? true
                                 :update-angle-fn cannon-target-fn
                                 :translate-x (- (/ (c/screen-to-world 12) 2))
                                 :translate-y (- (/ (c/screen-to-world 16) 2))
                                 :collider-type :multi))))

(defn- train-bolt-strip [pix-map x y w h]
  (doto pix-map
    (utils/pix-map-rect dark x y w h)
    (utils/pix-map-rect dark-highlight x (+ y (dec h)) w 1)
    (utils/pix-map-rect darkest x y w 1))
  (doseq [x (range x w 4)]
    (utils/pix-map-rect pix-map darkest x (inc y) 1 1)))

(defn- create-train-car-entity []
  (let [pix-map (pixmap* 128 128 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set train-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (utils/pix-map-rect pix-map (first color-set) x y w h)))
    (doseq [y [16 34 52]]
      (train-bolt-strip pix-map 0 y 36 4))
    (reset! train-car-entity (assoc (texture pix-map :set-region 0 0 train-car-width train-car-length)
                                    :render-layer 5
                                    :width train-car-width-adj
                                    :height train-car-length-adj
                                    :translate-x (- train-car-width-offset)
                                    :translate-y (- train-car-length-offset)
                                    :car? true))))

(defn create-textures []
  (create-train-car-entity)
  (create-cannon-entity))

(defn update-collider
  "Create a polygon, with rotation around x,y (the cars center).
   The polygon itself is a rectangle, centered at cx, cy which are
   offsets from x,y. l and w are 1/2 lengths and widths of a side,
   respectively"
  ([x y a {:keys [cx cy l w] :as c-map}]
   (update-collider x y cx cy a w l))
  ([x y cx cy a width-offset len-offset]
   (let [rx (+ x cx)
         ry (+ y cy)
         poly-verts (float-array [(- rx width-offset) (- ry len-offset)
                                  (+ rx width-offset) (- ry len-offset)
                                  (+ rx width-offset) (+ ry len-offset)
                                  (- rx width-offset) (+ ry len-offset)])
         poly (polygon poly-verts)]
     (polygon! poly :set-origin x y)
     (polygon! poly :set-rotation a)
     {:collider (polygon (polygon! poly :get-transformed-vertices))
      :cx cx
      :cy cy
      :l len-offset
      :w width-offset
      :collider-type :poly})))

(defn position-from-parent [{:keys [way-points-index] :as child} {:keys [x y id way-points] :as parent}]
  (assoc child
         :x (+ x (first (get way-points way-points-index)))
         :y (+ y (second (get way-points way-points-index)))
         :parent-id id))

(defn- updated-way-point [way-point angle]  
  (let [wp-x (first way-point)
        wp-y (second way-point)
        v (vector-2! (vector-2 wp-x wp-y) :rotate angle)]
    [(core/x v) (core/y v)])) ;(c/screen-to-world 1)))

(defn handle-test-bundle [screen {:keys [angle entities way-points] :as entity}]
    (assoc entity :angle (+ 0.3 (:angle entity))))

(defn handle-armament [screen entities {:keys [angle collider parent-id way-points-index enemy-ticks update-angle-fn] :or {enemy-ticks 1} :as entity}]
  (let [{:keys [way-points] :as parent} (first (filter #(= parent-id (:id %)) entities))
        p-x (:x parent)
        p-y (:y parent)
        p-angle (:angle parent)
        adjusted-way-point (updated-way-point (get way-points way-points-index) p-angle)
        new-x (+ p-x (first adjusted-way-point))
        new-y (+ p-y (second adjusted-way-point))]
    [(when (= 0 (mod enemy-ticks 180))
       (eb/start-rapid-fire screen entities entity))
       ;(eb/fire-turret-bullet screen x y angle))
     (let [entity (->> entity
                       (update-angle-fn entities parent))]
       (assoc entity
         :x new-x
         :y new-y
         :enemy-ticks (inc enemy-ticks)
         :collider (map #(update-collider new-x new-y angle %) collider)))]))    
  
(defn assign-track
  [train-car screen entities]
  (let [current-tracks (sort-by :at-ticks (->> entities
                                               (filter :track?)
                                               (filter #(= (:track-id %) :main-line))))
        track (:at-ticks (last current-tracks))
        track-entity (first (filter #(= track (:at-ticks %)) current-tracks))
        i-points (:i-points track-entity)
        i-point-index 0
        current-point (get i-points i-point-index)
        x (+ (:x track-entity) (:x current-point))
        y (+ (:y track-entity) (:y current-point))
        angle (:angle track-entity)];angle of track
    (assoc train-car
           :x (:x track-entity)
           :y (:y track-entity)
           :angle (:angle track-entity)
           :track track
           :track-id :main-line
           :i-point-index i-point-index)))

(defn assign-armaments
  [{:keys [x y angle] :as train-car} screen entities]
  (let [cx 0
        cy (/ train-car-length-offset 2)
        indestructible-guard (update-collider x
                                              y
                                              (- train-car-width-offset (/ indestructible-collider-width 2))
                                              0
                                              angle
                                              (/ indestructible-collider-width 2)
                                              (/ indestructible-collider-length 2))]
    (assoc train-car
           :collider [indestructible-guard]
           :collider-type :multi)))


(defn create-train-car
 ([screen entities]
  (let [uuid (c/uuid)
        train-car (-> @train-car-entity
                      (assoc :train? true
                             :enemy? true
                             :id uuid
                             :front? true
                             :way-points [[0 (/ cannon-side 2)]
                                          [(/ cannon-side 2) (/ cannon-side 2)]
                                          [(- (/ cannon-side 2)) (/ cannon-side 2)]
                                          [0 (- (/ cannon-side 2))]])
                      (assign-track screen entities)
                      (assign-armaments screen entities))
        cannon  (-> @cannon-entity
                    (assoc :angle 0
                           :id (c/uuid)
                           :way-points-index 0
                           :collider [(update-collider (:x train-car) (+ (:y train-car) (/ cannon-side 2))
                                                       0 0 (:angle train-car) (/ cannon-side 3) (/ cannon-side 3))])
                    (position-from-parent train-car))]
    [train-car cannon])))


(defn- next-track-entity [at-track tracks]
  (let [next-track (first (drop-while #(<= (:at-ticks %) at-track) tracks))]
    next-track))

(defn move-train [screen entities {:keys [collider i-point-index track track-id] :as entity}]
  (let [tracks (sort-by :at-ticks (->> entities
                                       (filter :track?)
                                       (filter #(= (:track-id %) track-id))))]
    (if-let [track-entity (first (filter #(= track (:at-ticks %)) tracks))]
      (let [top-tracks (drop-while #(>= track (:at-ticks %)) tracks)
            i-points (:i-points track-entity)
            current-point (get i-points i-point-index)
            x (+ (:x track-entity) (:x current-point))
            y (+ (:y track-entity) (:y current-point))
            angle (:angle track-entity);angle of track
            get-next (= (dec (count i-points)) i-point-index)
            next-track (next-track-entity track tracks)
            next-a (if (nil? next-track) 0.0 (:angle next-track))
            d-a (- next-a angle)
            angle-offset (* d-a (/ i-point-index (count i-points)))
            new-collider (map #(update-collider x y (+ angle angle-offset) %) collider)]
        (let [entity (assoc entity
                            :x x :y y :i-point-index (if get-next 0 (inc i-point-index)) :angle (+ angle-offset angle)
                            :track (if get-next (:at-ticks next-track) track) :collider new-collider)]
          (if (and (= 8 (count top-tracks)) (:front? entity))
            [(assoc entity :front? false) (create-train-car screen entities)]
            entity)))
      entity)))


