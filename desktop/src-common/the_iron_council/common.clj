(ns the-iron-council.common
  (:require [pixel-ships.bollinger :as bollinger :refer [color-scheme]]))

(def debug true)

(def ^:const s-to-w-divider 50.0)
(defn screen-to-world [x]
  (float (/ x s-to-w-divider)))

(def ^:const game-width 224.0)
(def ^:const game-height 288.0)
(def ^:const game-width-adj (screen-to-world game-width))
(def ^:const game-height-adj (screen-to-world game-height))

(def ^:const ship-b2d-width 2)
(def ^:const ship-b2d-height 3)
(def ^:const ship-y-start (screen-to-world (/ game-height 10)))
(def ^:const ship-mp-xoffset 6)
(def ^:const ship-mp-yoffset 10)

(def ^:const gunship-speed (screen-to-world 1.5))
(def ^:const yaw-change-amt 1.5)
(def ^:const yaw-reset-amt 1.75)

(defn hues-fn [seed]
  (vector (/ (bit-and seed 0xff) 1020.0)
          (/ (bit-shift-right (bit-and seed 0xff00) 8) 1020.0)
          (/ (bit-shift-right (bit-and seed 0xff0000) 16) 1020.0)
          (/ (bit-shift-right (bit-and seed 0xff000000) 24) 1020.0)))

(def gunship-color-scheme  (-> bollinger/color-scheme
                                (assoc :hues-fn hues-fn)
                                (assoc :bright-mid 0.36)
                                (assoc :bright-delta 0.11)))
