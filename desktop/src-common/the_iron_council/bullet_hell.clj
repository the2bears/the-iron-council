(ns the-iron-council.bullet-hell)

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

(defn wait
  [& {:keys [min-ticks max-ticks]
      :or {min-ticks 0 max-ticks Integer/MAX_VALUE}}]
  (fn [{:keys [ticks] :or {ticks 0} :as bullet}]
    (when (within-ticks? min-ticks ticks max-ticks)
      (assoc bullet
             :ticks (inc ticks)
             :dx 0.0
             :dy 0.0))))

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
