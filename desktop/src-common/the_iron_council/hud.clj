(ns the-iron-council.hud
  (:require [play-clj.core :refer [add-timer! clear! color defscreen game orthographic render! screen! stage update!]]
            [play-clj.g2d :refer [bitmap-font bitmap-font! texture]]
            [the-iron-council.common :as c]
            [the-iron-council.gunship :as gs])
  (:import [com.badlogic.gdx.graphics.g2d Batch BitmapFont]))

(def ^:const y-padding 4.0)
(def score-digits 8)
(def ^:const p1-1up-x 88.0)
(def ^:const p1-1up-y 4.0); 28.0);from top
(def ^:const p1-score-x 28.0)
(def ^:const p1-score-y 28.0)
(def ^:const logo-x 126.0)
(def ^:const logo-y 500.0)
(def ^:const game-over-x 246.0)
(def ^:const game-over-y 420.0)
(def ^:const game-paused-x 276.0)
(def ^:const game-paused-y 420.0)
(def ^:const high-score-label-x (/ 472.0 2.0)) ;(* 3 224)
(def ^:const high-score-x (/ 472.0 2.0))

(defn- add-mini-ships [n entities]
  (let [ships (for [ship (range n)
                    :let [x (- 600 (* ship 16))]]
                (assoc (gs/create-pixel-ship-texture Long/MAX_VALUE c/gunship-model c/gunship-color-scheme)
                       :width 16 :height 32
                       :x x :y 5
                       :id :pixel-ship :mini-ship? true))]
    (flatten (conj entities ships))))

(defn- count-mini-ships [screen entities]
  (let [actual-count (count (filter #(:mini-ship? %) entities))
        expected-count (- (:p1-lives screen) 1)]
    (cond (and (> expected-count -1) (not= expected-count actual-count))
          (->> entities
               (add-mini-ships expected-count))
          :else entities)))

(defn- pad-score [score]
  (let [padding (- score-digits (count (str score)))]
    (str (apply str (repeat padding " ")) score)))

(defscreen hud-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen
                          :renderer (stage)
                          :camera (orthographic :set-to-ortho false)
                          :p1-score 0
                          :high-score 0
                          :game-state :attract-mode
                          :font (bitmap-font "arcade20.fnt"))]
      entities))

  :on-render
  (fn [screen entities]
    (let [renderer (:renderer screen)
          ^Batch batch (.getBatch renderer)
          arcade-fnt (:font screen)
          show-logo? (= :attract-mode (:game-state screen))]
                                        ;entities (if show-logo?
                                        ;           (conj entities (assoc (texture "ds3_logo.png") :logo? true :width 180 :height 60 :x logo-x :y logo-y))
                                        ;           entities]
      (.begin batch)
      (bitmap-font! ^BitmapFont arcade-fnt :set-color (color :yellow))
      (bitmap-font! ^BitmapFont arcade-fnt :draw batch "1UP" p1-1up-x (- (game :height) y-padding))
      (bitmap-font! ^BitmapFont arcade-fnt :set-color (color :white))
      (bitmap-font! ^BitmapFont arcade-fnt :draw batch (str (pad-score (:p1-score screen))) p1-score-x (- (game :height) p1-score-y))
      (bitmap-font! ^BitmapFont arcade-fnt :set-color (color :red))
      (bitmap-font! ^BitmapFont arcade-fnt :draw batch "HIGH SCORE" high-score-label-x (- (game :height) y-padding))
      (bitmap-font! ^BitmapFont arcade-fnt :set-color (color :white))
      (bitmap-font! ^BitmapFont arcade-fnt :draw batch (str (pad-score (:high-score screen))) high-score-x (- (game :height) p1-score-y))
      (cond (= :game-over (:game-state screen))
            (do
              (bitmap-font! ^BitmapFont arcade-fnt :set-color (color :red))
              (bitmap-font! ^BitmapFont arcade-fnt :draw batch "GAME OVER" game-over-x game-over-y))
            (= :attract-mode (:game-state screen))
            (do
              (bitmap-font! ^BitmapFont arcade-fnt :set-color (color :white))
              (bitmap-font! ^BitmapFont arcade-fnt :draw batch "CHOOSE 1 OR 2 PLAYERS" logo-x game-over-y))
            (= :paused (:game-state screen))
            (do
              (bitmap-font! ^BitmapFont arcade-fnt :set-color (color :white))
              (bitmap-font! ^BitmapFont arcade-fnt :draw batch "PAUSED" game-paused-x game-paused-y)))
      (.end batch)
      (->> entities
           (count-mini-ships screen)
           (render! screen))))

  :on-update-score
  (fn [screen entities]
    (let [score (:p1-score screen)
          high-score (:high-score screen)]
      (update! screen :p1-score score :high-score high-score))
    nil)

  :on-update-lives
  (fn [screen entities]
    (let [lives (:p1-lives screen)]
      (update! screen :p1-lives lives))
    nil)

  :on-reset-score
  (fn [screen entities]
    (update! screen :p1-score 0)
    nil)

  :on-update-game-state
  (fn [screen entities]
    (let [state (:game-state screen)]
      (update! screen :game-state state)))

  :on-update-bonus
  (fn [screen entities]
    (let [value (:p1-bonus screen)]
      (update! screen :p1-bonus value))))
