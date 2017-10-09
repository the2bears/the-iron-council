(ns the-iron-council.enemy
  (:require [play-clj.core :refer [bundle color pixmap! pixmap* pixmap-format shape update! x y] :as core]
            [play-clj.g2d :refer [texture]]
            [play-clj.math :refer [circle circle! polygon polygon! rectangle rectangle! vector-2]]
            [the-iron-council.common :as c]
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


(def train-car-texture (atom nil))
(def darkest (color :black))
(def dark (color 0 29/255 71/255 1))
(def dark-highlight (color 55/255 75/255 91/255 1))

(def train-rects [[darkest [0 0 train-car-width train-car-length]]
                  ;[dark-highlight [1 1 34 70]]
                  ;[dark [2 2 32 68]]
                  [dark [1 1 34 70]]])

(defn- train-bolt-strip [pix-map x y w h]
  (doto pix-map
    (utils/pix-map-rect dark x y w h)
    (utils/pix-map-rect dark-highlight x (+ y (dec h)) w 1)
    (utils/pix-map-rect darkest x y w 1))
  (doseq [x (range x w 4)]
    (utils/pix-map-rect pix-map darkest x (inc y) 1 1)))

(defn- create-train-car-texture []
  (let [pix-map (pixmap* 128 128 (pixmap-format :r-g-b-a8888))]
    (doseq [color-set train-rects]
      (doseq [[x y w h] (partition 4 (second color-set))]
        (utils/pix-map-rect pix-map (first color-set) x y w h)))
    (doseq [y [16 34 52]]
      (train-bolt-strip pix-map 0 y 36 4))
    (texture pix-map :set-region 0 0 train-car-width train-car-length)))

(def test-side 16)

(defn- create-test-texture []
  (let [pix-map (pixmap* 32 32 (pixmap-format :r-g-b-a8888))]
    (utils/pix-map-rect pix-map (color :white) 0 0 test-side test-side)
    (texture pix-map :set-region 0 0 test-side test-side)))

(defn- update-collider
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

(defn create-test
 ([screen entities]
  (let [a 0
        translate-x (/ (- (c/screen-to-world test-side)) 2)
        translate-y (/ (- (c/screen-to-world test-side)) 1)
        train-car (-> (cond (nil? @train-car-texture)
                            (do
                              (reset! train-car-texture (create-train-car-texture))
                              @train-car-texture)
                            :else @train-car-texture)
                      (assoc ;:angle 10
                             :width train-car-width-adj
                             :height train-car-length-adj
                             :translate-x (- train-car-width-offset)
                             :translate-y (- train-car-length-offset)
                             :car? true))
        test-text (-> (create-test-texture)
                      (assoc :angle 0
                             :width (c/screen-to-world test-side)
                             :height (c/screen-to-world test-side)
                             :test-box? true
                             :translate-x translate-x
                             :translate-y translate-y
                             :origin-x (/ (c/screen-to-world test-side) 2)
                             :origin-y (/ (c/screen-to-world test-side) 2)))
        test-bundle (bundle train-car test-text)]
    (assoc train-car ;test-bundle
           :x (/ c/game-width-adj 2)
           :y (* 3 (/ c/game-height-adj 4))
           :angle a
           ;:bundle-angle a
           :test-bundle? true
           :way-points [[0 (/ (c/screen-to-world test-side) 2)]
                        [(/ (c/screen-to-world test-side) 2) (/ (c/screen-to-world test-side) 2)]
                        [(- (/ (c/screen-to-world test-side) 2)) (/ (c/screen-to-world test-side) 2)]
                        [0 (- (/ (c/screen-to-world test-side) 2))]]
           :render-layer 5))))

(defn handle-test-bundle [screen {:keys [angle entities] :as entity}]
  (let [entities (->> entities
                      (map (fn [entity]
                             (cond ;(:car? entity) (assoc entity :angle (+ (:angle entity) -0.2))
                                   ;(:test-box? entity) (assoc entity :angle (+ (:angle entity) 0.3))
                                   :else entity))))]

    (assoc entity :angle (+ 0.3 (:angle entity)) ;:bundle-angle (+ 0.3 (:angle entity)
           :entities entities)))

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
        car-collider (update-collider x y cx cy angle (/ train-car-width-offset 2) (/ train-car-length-offset 4))
        car-collider2 (update-collider x y cx (- cy) angle (/ train-car-width-offset 2) (/ train-car-length-offset 4))
        indestructible-guard (update-collider x
                                              y
                                              (- train-car-width-offset (/ indestructible-collider-width 2))
                                              0
                                              angle
                                              (/ indestructible-collider-width 2)
                                              (/ indestructible-collider-length 2))]
    (assoc train-car
           :collider [car-collider car-collider2 indestructible-guard]
           :collider-type :multi)))


(defn create-train-car
 ([screen entities]
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
        cx 0
        cy (/ train-car-length-offset 2)
        angle (:angle track-entity);angle of track
        train-car (cond (nil? @train-car-texture)
                        (do
                          (reset! train-car-texture (create-train-car-texture))
                          @train-car-texture)
                        :else @train-car-texture)
        car-collider (update-collider x y cx cy angle (/ train-car-width-offset 2) (/ train-car-length-offset 4))
        car-collider2 (update-collider x y cx (- cy) angle (/ train-car-width-offset 2) (/ train-car-length-offset 4))
        indestructible-guard (update-collider x
                                              y
                                              (- train-car-width-offset (/ indestructible-collider-width 2))
                                              0
                                              angle
                                              (/ indestructible-collider-width 2)
                                              (/ indestructible-collider-length 2))]
    (-> train-car
        (assoc :train? true
               :enemy? true
               :width train-car-width-adj
               :height train-car-length-adj
               :translate-x (- train-car-width-offset)
               :translate-y (- train-car-length-offset)
               :front? true
               :render-layer 5)
        (assign-track screen entities)
        (assign-armaments screen entities)))))

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


