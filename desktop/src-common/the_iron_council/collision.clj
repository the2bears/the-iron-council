(ns the-iron-council.collision
  (:require [play-clj.core :refer [update!]]
            [play-clj.math :refer [circle circle! intersector! polygon polygon! rectangle rectangle! vector-2 vector-2!]]
            [the-iron-council.explosion :as exp]))

(defmethod clojure.core/print-method com.badlogic.gdx.math.Polygon
  [p w]
  (print-method (into [] (polygon! p :get-vertices)) w))


(comment
   https://stackoverflow.com/questions/401847/circle-rectangle-collision-detection-intersection)

(defn- overlap
  "Tests for overlap with circle and polygon"
  [c p]
  (let [poly-vertices (into [] (partition 2  (into [] (polygon! p :get-vertices))))
        poly-vertices (conj poly-vertices (first poly-vertices))
        points-of-lines (map (fn[a b] [a b]) poly-vertices (rest poly-vertices))
        v1 (vector-2 0 0)
        v2 (vector-2 0 0)
        v3 (vector-2 (.x ^com.badlogic.gdx.math.Circle c) (.y ^com.badlogic.gdx.math.Circle c))
        radius (.radius ^com.badlogic.gdx.math.Circle c)
        radius2 (* radius radius)
        over-lap-line (reduce (fn[acc [[x1 y1][x2 y2]]] (or acc
                                                            (intersector! :intersect-segment-circle
                                                                          (vector-2! v1 :set x1 y1)
                                                                          (vector-2! v2 :set x2 y2)
                                                                          v3
                                                                          radius2)))
                              false
                              points-of-lines)
        overlap (or over-lap-line
                    (polygon! p :contains v3))]
    overlap))

(defn- compute-collision [bullet {:keys [collider-type] :as enemy}]
  (case collider-type
    :circle
    (let [collision? (intersector! :overlaps (:collider bullet) (:collider enemy))]
      (when collision?
        {:bullet bullet :enemy enemy :at (:collider enemy)}))
    :poly
    (let [overlaps? (intersector! :overlap-convex-polygons (:collider bullet) (:collider enemy))]
      (when overlaps?
        {:bullet bullet :enemy enemy :at (:collider enemy)}))
    :multi
    (for [e (:collider enemy)]
      (compute-collision bullet e))
    false))

(defn- enemy-bullet-gunship-collision? [{:keys [collider] :as enemy-bullet} gunship]
  (let [{:keys [x y r]} collider
        g-collider (:collider gunship)
        collision? (intersector! :overlaps (circle x y r) (circle (:x g-collider) (:y g-collider) (:r g-collider)))]
    collision?))

(defn compute-collisions [{:keys [ticks] :as screen} entities]
  (let [bullets (filter :bullet? entities)
        enemies (filter :enemy? entities)
        enemy-bullets (filter :enemy-bullet? entities)
        gunship (first (filter :gunship? entities))
        collision-with-gunship? (and gunship
                                     (reduce (fn[acc e-b]
                                               (or acc (enemy-bullet-gunship-collision? e-b gunship)))
                                             false
                                             enemy-bullets))
        collisions (remove nil? (flatten (for [bullet bullets
                                               enemy enemies]
                                            (if-some [collision (compute-collision bullet enemy)]
                                              collision))))]
    (when (not (empty? collisions))
      (update! screen :collisions collisions))
    (if collision-with-gunship?
      (conj (remove :gunship? entities) (exp/create-explosion (:x gunship) (:y gunship)))
      entities)))
