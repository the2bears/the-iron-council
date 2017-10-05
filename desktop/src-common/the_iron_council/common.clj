(ns the-iron-council.common
  (:require [pixel-ships.bollinger :as bollinger :refer [color-scheme]]
            [play-clj.core :refer [color key-pressed?]]))

(def debug true)

(def ^:const s-to-w-divider 50.0)
(defn screen-to-world [x]
  (float (/ x s-to-w-divider)))
(defn uuid [] (str (java.util.UUID/randomUUID)))

(def ^:const game-width 224.0)
(def ^:const game-height 288.0)
(def ^:const game-width-adj (screen-to-world game-width))
(def ^:const game-height-adj (screen-to-world game-height))

(def ^:const ship-b2d-width 2)
(def ^:const ship-b2d-height 3)
(def ^:const ship-y-start (screen-to-world (/ game-height 10)))
(def ^:const ship-mp-xoffset 6)
(def ^:const ship-mp-yoffset 26)
(def ^:const ship-option-width 6)
(def ^:const ship-option-buffer 1)
(def ^:const ship-option-yoffset 14)
(def ^:const ship-option-xoffset-left (+ ship-option-width ship-mp-xoffset ship-option-buffer))
(def ^:const ship-option-xoffset-right (+ ship-mp-xoffset ship-option-buffer))

(def ^:const gunship-speed (screen-to-world 1.5))
(def ^:const gunship-xy-speed (Math/sqrt (/ (* gunship-speed gunship-speed) 2)))
(def ^:const gunship-xy-ratio (/ gunship-xy-speed gunship-speed))
(def ^:const yaw-with-x false)
(def ^:const yaw-change-amt 1.5)
(def ^:const yaw-reset-amt 1.75)
(def ^:const yaw-delta-max 30)

(def ^:const bullet-width (screen-to-world 6))
(def ^:const bullet-height (screen-to-world 14))
(def ^:const bullet-speed (screen-to-world 5))
(def ^:const bullet-half-width (/ bullet-width 2))
(def ^:const bullet-half-height (/ bullet-height 2))
(def ^:const bullet-hitbox-x bullet-half-width)
(def ^:const bullet-hitbox-y (/ (+ bullet-height bullet-half-height) 2))
(def ^:const bullet-hitbox-side (screen-to-world 2))
(def bullet-rects [[(color :orange) [2 0 2 14 1 1 4 9 0 2 6 4]]
                   [(color :yellow) [2 1 2 9]]
                   [(color :white) [1 2 4 4]]])                                        ;
(def ^:const gatling-shell-width (screen-to-world 2))
(def ^:const gatling-shell-height (screen-to-world 8))
(def ^:const gatling-shell-speed (screen-to-world 6))
(def ^:const gatling-shell-half-width (/ gatling-shell-width 2))
(def ^:const gatling-shell-half-height (/ gatling-shell-height 2))
(def ^:const gatling-hitbox-x gatling-shell-half-width)
(def ^:const gatling-hitbox-y (/ (+ gatling-shell-height gatling-shell-half-height) 2))
(def ^:const gatling-hitbox-side (screen-to-world 1))
(def ^:const gatling-shell-xoffset-left (- ship-option-xoffset-left 2))
(def ^:const gatling-shell-xoffset-right (- gatling-shell-xoffset-left 2))
(def gatling-shell-rects [[(color :orange) [0 6 2 2]]
                          [(color 1.0 0.75 0 1) [0 4 2 2]];yellow-orange
                          [(color :yellow) [0 2 2 2]]
                          [(color :white) [0 0 2 2]]])
(def ^:const rocket-width (screen-to-world 3))
(def ^:const rocket-height (screen-to-world 4))
(def ^:const rocket-speed (screen-to-world 3))
(def ^:const rocket-half-width (/ gatling-shell-width 2))
(def ^:const rocket-half-height (/ gatling-shell-height 2))
(def ^:const rocket-hitbox-x gatling-shell-half-width)
(def ^:const rocket-hitbox-y (/ (+ gatling-shell-height gatling-shell-half-height) 2))
(def ^:const rocket-hitbox-side (screen-to-world 1))
(def ^:const rocket-xoffset-left (- ship-option-xoffset-left 2))
(def ^:const rocket-xoffset-right (- rocket-xoffset-left 3))
(def rocket-rects [[(color :red) [1 2 1 2 0 3 3 1]]
                   [(color :white) [1 0 1 2]]])

(def ^:const refresh-cannon 0.2)
(def ^:const refresh-gatling 0.08)
(def ^:const refresh-rocket 0.6)

(def ^:const oob-padding (screen-to-world 10))

(def ^:const cannon-key :x)
(defn cannon-key-pressed? []
  (key-pressed? :x))

(defn hues-fn [seed]
  (vector (/ (bit-and seed 0xff) 1020.0)
          (/ (bit-shift-right (bit-and seed 0xff00) 8) 1020.0)
          (/ (bit-shift-right (bit-and seed 0xff0000) 16) 1020.0)
          (/ (bit-shift-right (bit-and seed 0xff0000) 16) 1020.0)))

(def gunship-color-scheme  (-> bollinger/color-scheme
                                (assoc :hues-fn hues-fn)
                                (assoc :bright-mid 0.36)
                                (assoc :bright-delta 0.11)
                                (assoc :sat-multipliers [ -2 -1  0 1 0 -1 0 1 2 1 0 -1 0 0 0 -1 0 -1 0 -1 0 -1 0 -1])))

(defn- add-cells [model]
  (let [hull ((comp :hull :model) model)
        new-hull  (into hull [{:x 5 :y 11} {:x 5 :y 12}
                              {:x 5 :y 13} {:x 5 :y 14}
                              {:x 5 :y 15} {:x 5 :y 16}
                              {:x 5 :y 17} {:x 5 :y 18}
                              {:x 5 :y 19} {:x 5 :y 20}
                              {:x 4 :y 19}])]
    (-> model
        (assoc-in [:model :hull] new-hull)
        (assoc :ship-y 24))))

(def gunship-model (add-cells bollinger/model))

(def gatling-model
  {:name :gatlin
   :seed Integer/MAX_VALUE
   :ship-size 8
   :ship-x 6
   :ship-y 8
   :model {:solid [{:x 1, :y 0} {:x 2, :y 0} {:x 0, :y 3} {:x 1, :y 3} {:x 2, :y 3}]
           :hull [{:x 2, :y 1} {:x 2, :y 2} {:x 2, :y 4} {:x 2, :y 5} {:x 2, :y 6} {:x 1, :y 4} {:x 1, :y 5}]}})

(def rocket-model
  {:name :rocket
   :seed Integer/MAX_VALUE
   :ship-size 8
   :ship-x 6
   :ship-y 8
   :model {:solid [{:x 0, :y 0} {:x 0, :y 4} {:x 1, :y 4} {:x 2, :y 4}]
           :hull [{:x 1, :y 1} {:x 1, :y 2} {:x 1, :y 3} {:x 2, :y 1} {:x 2, :y 2} {:x 2, :y 3} {:x 2, :y 5} {:x 2, :y 6}]}})

