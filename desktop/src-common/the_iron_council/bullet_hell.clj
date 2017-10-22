(ns the-iron-council.bullet-hell
  (:require [play-clj.math :refer [circle circle! vector-2 vector-2!]]
            [play-clj.core :refer [x y] :as core]
            [the-iron-council.common :as c]))

(defn- within-ticks?
  "Simple predicate for checking within the range, inclusive of the lower bounds, exclusive of the upper one."
  [min-ticks ticks max-ticks]
  (< (dec min-ticks) ticks max-ticks))

(defn linear-movement
  "Simple linear movement, from min-ticks (default 0) to max-ticks (default Integer/MAX_VALUE).
   Each tick dx and dy ('d'eltas) are added to the bullet's x and y respectively. dx and dy are
   added to the bullet's map."
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
  "Continues in linear movement, from min-ticks (default 0) to max-ticks (default Integer/MAX_VALUE).
   Each tick dx and dy ('d'eltas) are added to the bullet's x and y respectively. dx and dy are
   obtained from the bullet's map, so they must be present already."
  [& {:keys [min-ticks max-ticks]
      :or {min-ticks 0 max-ticks Integer/MAX_VALUE}}]
  (fn [{:keys [x y dx dy ticks] :or {ticks 0} :as bullet}]
    (when (within-ticks? min-ticks ticks max-ticks)
      (assoc bullet
             :x (+ x dx)
             :y (+ y dy)
             :ticks (inc ticks)))))

(defn accel
  "Simple acceleration, from min-ticks (default 0) to max-ticks (default Integer/MAX_VALUE).
   Each tick ax and ay ('a'cceleration) are added to dx and dy ('d'eltas) respectively.
   Subsequently dx and dy are added to the bullet's x and y. dx and dy are added to the bullet's
   map."
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

(defn rotate
  "Simple rotation, from min-ticks (default 0) to max-ticks (default Integer/MAX_VALUE).
   Each tick the vector of (dx, dy) is rotated by angle da. dx and dy are subsequently added to the
   bullet's x and y respectively, and the bullet's map is updated with the new dx and dy values."
  [& {:keys [da min-ticks max-ticks]
      :or {min-ticks 0 max-ticks Integer/MAX_VALUE}}]
  (let [term (- max-ticks min-ticks)]
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

(defn change-direction
  "Changes the bullets direction, to the target angle, in n terms (from min-ticks to max-ticks).
   Up is 90, right is 0. sx and sy represent the starting velocity and are used to calculate the
   starting angle of the bullet. Each tick then sees the dx and dy values recalculated by rotating
   the vector. These values are updated in the bullet's map."
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
  "Simple change in velocity, from min-ticks (default 0) to max-ticks (default Integer/MAX_VALUE).
   sx/sy represent the 's'tarting velocity, tx/ty the 't'arget velocity. ax and ay are calculated
   and subsequently added to dx and dy each tick. These are added the the bullet's x and y respectively
   and updated in the bullet's map."
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

(defn split
  [& {:keys [dx dy da min-ticks max-ticks]
      :or {da 0.0 min-ticks 0 max-ticks Integer/MAX_VALUE}}]
  (let [v1 (vector-2 dx dy :rotate da)
        v2 (vector-2 dx dy :rotate (- da))]
    (fn [{:keys [x y dx dy ticks collider bullet-hell-fn] :or {ticks 0} :as bullet}]
      (when (within-ticks? min-ticks ticks max-ticks)
        (let [b1 (assoc bullet
                        :id (c/uuid)
                        :x x
                        :y y
                        :dx (core/x v1)
                        :dy (core/y v1)
                        :ticks (inc ticks)
                        :bullet-hell-fn bullet-hell-fn)
              b2 (assoc bullet
                        :id (c/uuid)
                        :x x
                        :y y
                        :dx (core/x v2)
                        :dy (core/y v2)
                        :ticks (inc ticks)
                        :bullet-hell-fn bullet-hell-fn)]
          [b1 b2])))))
