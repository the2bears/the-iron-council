(ns the-iron-council.utils
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]))

(defn pix-map-rect [pix-map c x y w h]
  (doto pix-map
    (pixmap! :set-color c)
    (pixmap! :fill-rectangle x y w h)))

(defn position-from-parent [{:keys [way-points-index] :as child} {:keys [x y id way-points] :as parent}]
  (assoc child
         :x (+ x (first (get way-points way-points-index)))
         :y (+ y (second (get way-points way-points-index)))
         :parent-id id))


