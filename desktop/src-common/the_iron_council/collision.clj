(ns the-iron-council.collision
  (:require [play-clj.core :refer [update!]]
            [play-clj.math :refer [circle circle! intersector! polygon polygon! rectangle rectangle!]]))

(defn compute-collisions [{:keys [ticks] :as screen} entities]
  (let [bullets (filter :bullet? entities)
        enemies (filter :enemy? entities)
        ;colliders (filter :collider entities)
        collisions (for [bullet bullets
                         enemy enemies
                         :when (or (intersector! :overlaps (:collider bullet) (:collider enemy))
                                   (intersector! :overlaps (:collider' bullet) (:collider enemy)))]
                     [bullet enemy])]
    (when (not (empty? collisions))
      (update! screen :collisions collisions)))
  entities)



(def cir (circle 5 5 5))

;corner point is bottom-left
(def rec (rectangle 10 0 10 10))



(def poly (polygon (float-array [10.0 0.0 10.0 10.0 20.0 10.0 20.0 0.0])))
(polygon! poly :set-origin 15 5)
(polygon! poly :set-rotation -10)

(def verts (polygon! poly :get-transformed-vertices))

(partition 2 verts)

(intersector! :overlaps cir rec)

(for [x (filter even? [1 2 3 4 5 6 7 8])]
  (prn :x x))

(filter (comp some? :c) [{:a 1} {:b 2} {:c 3}])
(filter :c [{:a 1} {:b 2} {:c 3}]) 

(remove #(some? (#{1 2 3} %)) [1 2 3 4 5 6 7])
