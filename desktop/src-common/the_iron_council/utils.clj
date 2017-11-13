(ns the-iron-council.utils
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]
            [play-clj.math :refer [polygon polygon!]]))

(defn pix-map-rect [pix-map c x y w h]
  (doto pix-map
    (pixmap! :set-color c)
    (pixmap! :fill-rectangle x y w h)))

(defn position-from-parent [{:keys [way-points-index] :as child} {:keys [x y id way-points] :as parent}]
  (assoc child
         :x (+ x (first (get way-points way-points-index)))
         :y (+ y (second (get way-points way-points-index)))
         :parent-id id))

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


