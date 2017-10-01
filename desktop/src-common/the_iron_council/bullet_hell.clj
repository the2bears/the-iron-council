(ns the-iron-council.bullet-hell
  (:require [play-clj.math :refer [vector-2 vector-2!]]
            [play-clj.core :refer [x y] :as core]))

(defn- within-ticks?
  "Simple predicate for checking within the range, inclusive of the lower bounds, exclusive of the upper one."
  [min-ticks ticks max-ticks]
  (< (dec min-ticks) ticks max-ticks))

(defn linear-movement
  [& {:keys [dx dy min-ticks max-ticks]
      :or {dx 0.0 dy 0.0 min-ticks 0 max-ticks Integer/MAX_VALUE}}]
  (fn [{:keys [x y ticks] :or {ticks 0} :as bullet}]
    (when (within-ticks? min-ticks ticks max-ticks)
      (assoc bullet
             :x (+ x dx)
             :y (+ y dy)
             :dx dx
             :dy dy
             :ticks (inc ticks)))))

(defn continue
  [& {:keys [min-ticks max-ticks]
      :or {min-ticks 0 max-ticks Integer/MAX_VALUE}}]
  (fn [{:keys [x y dx dy ticks] :or {ticks 0} :as bullet}]
    (when (within-ticks? min-ticks ticks max-ticks)
      (assoc bullet
             :x (+ x dx)
             :y (+ y dy)
             :ticks (inc ticks)))))

(defn accel
  [& {:keys [ax ay min-ticks max-ticks]
      :or {min-ticks 0 max-ticks Integer/MAX_VALUE}}]
  (fn [{:keys [x y dx dy ticks] :or {dx 0 dy 0 ticks 0} :as bullet}]
    (when (within-ticks? min-ticks ticks max-ticks)
      (let [dx (+ dx ax)
            dy (+ dy ay)]
        (assoc bullet
               :x (+ x dx)
               :y (+ y dy)
               :dx dx
               :dy dy
               :ticks (inc ticks))))))

(defn change-direction
  "Changes the bullets direction, to the target angle, in n terms. Up is 90, right is 0."
  [& {:keys [sx sy ta min-ticks max-ticks]
      :or {min-ticks 0 max-ticks Integer/MAX_VALUE}}]
  (let [term (- max-ticks min-ticks)
        sa (vector-2! (vector-2 sx sy) :angle)
        da (/ (- ta sa) term)]
    (fn [{:keys [x y dx dy ticks] :or {ticks 0} :as bullet}]
      (when (within-ticks? min-ticks ticks max-ticks)
        (let [v (vector-2! (vector-2 dx dy) :rotate da)
              dx (core/x v)
              dy (core/y v)]
          (assoc bullet
                 :x (+ x dx)
                 :y (+ y dy)
                 :dx dx
                 :dy dy
                 :ticks (inc ticks)))))))

(defn change-speed
  [& {:keys [sx sy tx ty min-ticks max-ticks]
      :or {sx 0.0 sy 0.0 tx 0.0 ty 0.0 min-ticks 0 max-ticks Integer/MAX_VALUE}}]
  (let [term (- max-ticks min-ticks)
        ax (/ (- tx sx) term)
        ay (/ (- ty sy) term)]
    (fn [{:keys [x y dx dy ticks] :or {dx sx dy sy ticks 0} :as bullet}]
      (when (within-ticks? min-ticks ticks max-ticks)
        (let [dx (+ dx ax)
              dy (+ dy ay)]
          (assoc bullet
                 :x (+ x dx)
                 :y (+ y dy)
                 :dx dx
                 :dy dy
                 :ticks (inc ticks)))))))
