(ns the-iron-council.common
  (:require [pixel-ships.bollinger :as bollinger :refer [color-scheme]]
            [play-clj.core :refer [color key-pressed?]]))

(def debug false)

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
(def ^:const ship-mp-yoffset 26)

(def ^:const gunship-speed (screen-to-world 1.5))
(def ^:const gunship-xy-speed (Math/sqrt (/ (* gunship-speed gunship-speed) 2)))
(def ^:const gunship-xy-ratio (/ gunship-xy-speed gunship-speed))
(def ^:const yaw-with-x false)
(def ^:const yaw-change-amt 1.5)
(def ^:const yaw-reset-amt 1.75)
(def ^:const yaw-delta-max 30)

(def ^:const bullet-width (screen-to-world 6))
(def ^:const bullet-height (screen-to-world 14))
(def ^:const bullet-speed (screen-to-world 300))
(def ^:const bullet-half-width (/ bullet-width 2))
(def ^:const bullet-half-height (/ bullet-height 2))
(def ^:const bullet-hitbox-x bullet-half-width)
(def ^:const bullet-hitbox-y (/ (+ bullet-height bullet-half-height) 2))
(def ^:const bullet-hitbox-side (screen-to-world 2))
(def bullet-rects [[(color :orange) [2 0 2 14 1 1 4 9 0 2 6 4]]
                   [(color :yellow) [2 1 2 9]]
                   [(color :white) [1 2 4 4]]])                                        ;

(def ^:const oob-padding (screen-to-world 30))
(def ^:const oob-x-length (+ game-width-adj (* 2 oob-padding)))
(def ^:const oob-y-length (+ game-height-adj (* 2 oob-padding)))

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
        new-hull  (into [] (concat hull [{:x 5 :y 11} {:x 5 :y 12}
                                         {:x 5 :y 13} {:x 5 :y 14}
                                         {:x 5 :y 15} {:x 5 :y 16}
                                         {:x 5 :y 17} {:x 5 :y 18}
                                         {:x 5 :y 19} {:x 5 :y 20}
                                         {:x 4 :y 19}]))] ;{:x 5 :y 22}]))]
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

