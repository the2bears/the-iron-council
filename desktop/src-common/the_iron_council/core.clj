(ns the-iron-council.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d-physics :refer [add-body! body! body-def body-position! box-2d chain-shape first-entity fixture-def second-entity step!]]
            [the-iron-council.bullet :as bullet]
            [the-iron-council.common :as c]
            [the-iron-council.gunship :refer :all :as gs])
  (:import [com.badlogic.gdx.physics.box2d Box2DDebugRenderer]))

(defn on-new-game [screen entities]
  (update! screen
           :game-state :in-game
           :ticks 0)
  (conj entities (gs/create-ship-entity! screen)))

(defn handle-all-entities [screen entities]
  (->> entities
       (map (fn [entity]
              (cond (:gunship? entity) (gs/move-player-tick screen entities entity)
                    :else entity)))))

(defn check-for-input [screen entities]
  (case (:game-state screen)
    :in-game
    (cond
      (and (get screen :fire-when-ready true)
           (key-pressed? :x))
      (if-let [gunship (first (filter #(:gunship? %) entities))]
        (let [x (:x gunship)
              y (:y gunship)
              a (:angle gunship)]
          (update! screen :fire-when-ready false)
          (add-timer! screen :refresh-shot 0.2)
          (conj entities (bullet/create-bullet! screen x (+ 0.1 y) a)))
        entities)
      :else entities)
    entities))

(defn create-oob-body!
  [screen width height]
  (let [body (add-body! screen (body-def :static))]
    (->> [0 0
          0 height
          width height
          width 0
          0 0]
         float-array
         (chain-shape :create-chain)
         (fixture-def
          :is-sensor true
          :density 1 :restitution 1 :shape)
         (body! body :create-fixture))
    body))

(defn create-oob-entity!
  [screen width height]
  (let [rect (bundle nil)]
    (assoc rect
           :body (create-oob-body! screen width height)
           :width width :height height)))

(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen
                          :renderer (stage)
                          :camera (orthographic :set-to-ortho false (c/screen-to-world c/game-width) (c/screen-to-world c/game-height))
                          :world (box-2d 0 0);-2.0)
                          :game-state :attract-mode
                          :ticks 0
                          :fire-when-ready true
                          :debug-renderer (Box2DDebugRenderer.))
          top-oob (doto
                    (create-oob-entity! screen c/oob-x-length c/oob-padding)
                    (body-position! (- c/oob-padding) (+ c/game-height-adj c/oob-padding) 0))
          bottom-oob (doto
                       (create-oob-entity! screen c/oob-x-length c/oob-padding)
                       (body-position! (- c/oob-padding) (- (* 2 c/oob-padding)) 0))
          left-oob (doto
                      (create-oob-entity! screen c/oob-padding c/oob-y-length)
                      (body-position! (- (* 2 c/oob-padding)) (- c/oob-padding) 0))
          right-oob (doto
                       (create-oob-entity! screen c/oob-padding c/oob-y-length)
                       (body-position! (+ c/game-width-adj c/oob-padding) (- c/oob-padding) 0))]
      [(assoc top-oob :id :top-oob :oob? true :render-layer 0)
       (assoc bottom-oob :id :bottom-oob :oob? true :render-layer 0)
       (assoc left-oob :id :left-oob :oob? true :render-layer 0)
       (assoc right-oob :id :right-oob :oob? true :render-layer 0)]))
  
  :on-render
  (fn [screen entities]
    (let [debug-renderer (:debug-renderer screen)
          world (:world screen)
          camera (:camera screen)
          ticks (:ticks screen)
          game-state (:game-state screen)]
      (clear! 0.1 0.1 0.12 1)
      (cond (not= :paused game-state)
            (do
              (update! screen :ticks (inc (:ticks screen)))
              (let [entities
                    (->> entities
                         (step! screen)
                         (check-for-input screen)
                         (handle-all-entities screen)
                         (sort-by :render-layer)
                         (render! screen))]

                (if c/debug
                  (.render debug-renderer world (.combined camera)))
                entities))
            :else (->> entities
                       ;(check-game-status screen)
                       (render! screen)))))


  :on-key-up
  (fn [screen entities]
    (case (:game-state screen)
      :attract-mode
      (cond (= (:key screen) (key-code :num-1))
            (on-new-game screen entities))
      :in-game
      (cond (= (:key screen) (key-code :p))
            (do
              (update! screen :game-state :paused)
              entities))
      :paused
      (cond (= (:key screen) (key-code :p))
            (do
              (update! screen :game-state :in-game)
              entities))
      entities))

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :refresh-shot (do
                      (update! screen :fire-when-ready true)
                      entities)))

  :on-begin-contact
  (fn [screen entities]
    (let [entity (first-entity screen entities)
          entity2 (second-entity screen entities)]
      ;(prn :entity (:id entity) :entity2 (:id entity2))
      (cond
        (:bullet? entity) (bullet/handle-collision entity entity2 screen entities)
        (:bullet? entity2) (bullet/handle-collision entity2 entity screen entities)))))

(defgame the-iron-council-game
  :on-create
  (fn [this]
    (set-screen! this main-screen)
    (graphics! :set-v-sync true)))

(-> main-screen :entities deref)
