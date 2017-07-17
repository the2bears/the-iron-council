(ns the-iron-council.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! box-2d chain-shape first-entity fixture-def second-entity step!]]
            [the-iron-council.common :as c]
            [the-iron-council.gunship :refer :all :as gs])
  (:import [com.badlogic.gdx.physics.box2d Box2DDebugRenderer]))

(defn on-new-game [screen entities]
  (update! screen
           :game-state :attract-mode
           :ticks 0)
  (conj entities (gs/create-ship-entity! screen)))

(defn handle-all-entities [screen entities]
  (->> entities
       (map (fn [entity]
              (cond (:gunship? entity) (gs/move-player-tick screen entities entity)
                    :else entity)))))
       
(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen
                          :renderer (stage)
                          :camera (orthographic :set-to-ortho false (c/screen-to-world c/game-width) (c/screen-to-world c/game-height))
                          :world (box-2d 0 0);-2.0)
                          :game-state :attract-mode
                          :ticks 0
                          :debug-renderer (Box2DDebugRenderer.))]))
  
  :on-render
  (fn [screen entities]
    (let [debug-renderer (:debug-renderer screen)
          world (:world screen)
          camera (:camera screen)
          ticks (:ticks screen)
          game-state (:game-state screen)]
      (clear! 0.1 0.1 0.12 1)
      (do
        (update! screen :ticks (inc (:ticks screen)))
        (let [entities
              (->> entities
                   (step! screen)
                   (handle-all-entities screen)
                   (sort-by :render-layer)
                   (render! screen))]
          (if c/debug
            (.render debug-renderer world (.combined camera)))
          entities))))


  :on-key-up
  (fn [screen entities]
    (case (:game-state screen)
      :attract-mode
      (cond (= (:key screen) (key-code :num-1))
            (on-new-game screen entities))
      entities)))

(defgame the-iron-council-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)
    (graphics! :set-v-sync true)))
