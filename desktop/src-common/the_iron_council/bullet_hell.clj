(ns the-iron-council.bullet-hell)

(defn- within-ticks?
  "Simple predicate for checking within the range, inclusive of the lower bounds, exclusive of the upper one."
  [min-ticks ticks max-ticks]
  (< (dec min-ticks) ticks max-ticks))

(defn linear-movement
  [& {:keys [dx dy min-ticks max-ticks]
      :or {dx 0 dy 0 min-ticks 0 max-ticks Integer/MAX_VALUE}}]
  (fn [{:keys [x y ticks] :or {ticks 0} :as bullet}]
    (when (within-ticks? min-ticks ticks max-ticks)
      (assoc bullet
             :x (+ x dx)
             :y (+ y dy)
             :ticks (inc ticks)))))
