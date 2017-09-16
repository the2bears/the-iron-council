(ns the-iron-council.utils
  (:require [play-clj.core :refer [color pixmap! pixmap* pixmap-format shape x y] :as core]))

(defn pix-map-rect [pix-map c x y w h]
  (doto pix-map
    (pixmap! :set-color c)
    (pixmap! :fill-rectangle x y w h)))
