(ns the-iron-council.core
  (:require [play-clj.core :refer :all]
            [play-clj.g2d :refer [bitmap-font bitmap-font!]]
            [the-iron-council.bullet :as bullet]
            [the-iron-council.collision :as collision]
            [the-iron-council.common :as c]
            [the-iron-council.debug-renderer :as debugger]
            [the-iron-council.enemy :as enemy]
            [the-iron-council.enemy-bullet :as eb]
            [the-iron-council.explosion :as exp]
            [the-iron-council.gunship :refer :all :as gs]
            [the-iron-council.hud :as hud]
            [the-iron-council.snow :as snow]
            [the-iron-council.track :refer [create-curved-track create-track-entity] :as tr])
  (:import [com.badlogic.gdx.graphics.g2d Batch BitmapFont]
           [com.badlogic.gdx.graphics.glutils ShapeRenderer]))


(def ^:const logo-x (c/screen-to-world 126.0))
(def ^:const logo-y (c/screen-to-world 500.0))
(def ^:const game-over-x (c/screen-to-world 246.0))
(def ^:const game-over-y (c/screen-to-world 420.0))

(defn on-new-game [screen entities]
  (update! screen
           :game-state :in-game
           :ticks 0)
  (tr/create-curved-track screen)
  (-> entities
      (conj (gs/create-ship-entity! screen))))
  
(defn handle-all-entities [screen entities]
  (->> entities
       (map (fn [entity]
              (cond (:gunship? entity) (gs/move-player-tick screen entities entity)
                    (:bullet? entity) (bullet/move-bullet screen entity)
                    (:enemy-bullet? entity) (eb/handle-bullet screen entity)
                    ;(:enemy? entity) (enemy/move-enemy screen entity)
                    (or (:track? entity) (:rail? entity)) (tr/move-track screen entity)
                    (:explosion? entity) (exp/handle-explosion entity)
                    (:train? entity) (enemy/move-train screen entities entity)
                    (:snow? entity) (snow/move-snow screen entity)
                    :else entity)))))

(defn handle-collisions [{:keys [collisions] :as screen} entities]
  (let [bullet-ids (into #{} (map (comp :id :bullet) collisions))]
    (if (empty? bullet-ids)
      entities
      (do
        (update! screen :collisions [])
        (let [updated-entities (remove #(some? (bullet-ids (:id %))) entities)
              bullets (filter #(some? (bullet-ids (:id %))) entities)
              explosions (map (fn[bullet](exp/create-explosion (+ (:x bullet) (:c-x-offset bullet)) (+ (:y bullet) (:c-y-offset bullet)))) bullets)]
          (into updated-entities explosions))))))

(defn check-for-input [{:keys [game-state option-type] :as screen} entities]
  (case (:game-state screen)
    :in-game
    (cond
      (and (get screen :fire-cannon-when-ready true)
           (c/cannon-key-pressed?)) ;(key-pressed? :x))
      (if-let [gunship (first (filter #(:gunship? %) entities))]
        (let [x (:x gunship)
              y (:y gunship)
              a (:angle gunship)]
          (update! screen :fire-cannon-when-ready false)
          (add-timer! screen :refresh-cannon-shot c/refresh-cannon)
          (into entities (list (bullet/fire-cannon! screen x y a))))
        entities)
      (and (get screen :fire-gatling-when-ready true)
           (c/cannon-key-pressed?) ;(key-pressed? :x))
           (= option-type :gatling))
      (if-let [gunship (first (filter #(:gunship? %) entities))]
        (let [x (:x gunship)
              y (:y gunship)
              a (:angle gunship)]
          (update! screen :fire-gatling-when-ready false)
          (add-timer! screen :refresh-gatling-shot c/refresh-gatling)
          (into entities (bullet/fire-gatling! screen x y a)))
        entities)
      (and (get screen :fire-rocket-when-ready true)
           (c/cannon-key-pressed?) ;(key-pressed? :x))
           (= option-type :rocket))
      (if-let [gunship (first (filter #(:gunship? %) entities))]
        (let [x (:x gunship)
              y (:y gunship)
              a (:angle gunship)]
          (update! screen :fire-rocket-when-ready false)
          (add-timer! screen :refresh-rocket-shot c/refresh-rocket)
          (into entities (bullet/fire-rocket! screen x y a)))
        entities)
      :else entities)
    entities))

(defn- check-game-status [screen entities]
  ;(screen! hud/hud-screen :on-update-lives :p1-lives lives)
  ;(screen! hud/hud-screen :on-update-score :p1-score (:p1-level-score screen) :high-score high-score)
  (screen! hud/hud-screen :on-update-game-state :game-state (:game-state screen))
  entities)


(defscreen main-screen
  :on-show
  (fn [screen entities]
    (let [screen (update! screen
                          :renderer (stage)
                          :shape-renderer (ShapeRenderer.)
                          :camera (orthographic :set-to-ortho false (c/screen-to-world c/game-width) (c/screen-to-world c/game-height))
                          :game-state :attract-mode
                          :ticks 0
                          :option-type :gatling
                          :fire-cannon-when-ready true
                          :fire-gatling-when-ready true
                          :fire-rocket-when-ready true
                          :font (bitmap-font "arcade20.fnt")
                          :debug true)]
          ;snow (snow/create-snow)]
      []))
       ;snow]))

  :on-render
  (fn [{:keys [debug] :as screen} entities]
    (let [camera (:camera screen)
          ticks (:ticks screen)
          game-state (:game-state screen)]
      (clear! 0.1 0.1 0.12 1)
      (cond (not (= :paused game-state))
            (do
              ;(Thread/sleep 50)
              (update! screen :ticks (inc (:ticks screen)))
              (let [entities
                    (->> entities
                         (check-for-input screen)
                         (handle-all-entities screen);move done here
                         (flatten)
                         (tr/add-tracks screen)
                         ;(enemy-test screen)
                         (collision/compute-collisions screen);collisions detected
                         (handle-collisions screen);collisions handled
                         (check-game-status screen)
                         (sort-by :render-layer)
                         (render! screen))]
                (if debug
                  (debugger/render screen entities))
                entities))
            :else
            (let [entities
                  (->> entities
                       (check-game-status screen)
                       (render! screen))]
               (if debug
                 (debugger/render screen entities))
               entities))))

  :on-key-up
  (fn [{:keys [debug option-type key] :as screen} entities]
    (case (:game-state screen)
      :attract-mode
      (cond (= (:key screen) (key-code :num-1))
            (on-new-game screen entities)
            (= (:key screen) (key-code :o))
            (do
              (update! screen :option-type (if (= option-type :gatling) :rocket :gatling))
              entities)
            (= (:key screen) (key-code :h))
            (let [eb (eb/test-bullet! screen 50 50 0)]
              (conj entities eb)))
      :in-game
      (cond (= (:key screen) (key-code :p))
            (do
              (update! screen :game-state :paused)
              entities)
            (= (:key screen) (key-code :x))
            (do
              (update! screen :fire-cannon-when-ready true)
              (update! screen :fire-gatling-when-ready true)
              ;(update! screen :fire-rocket-when-ready true)
              entities)
            (= (:key screen) (key-code :e))
            (let [car (enemy/create-train-car screen entities)]
              (conj entities car))
            (= (:key screen) (key-code :d))
            (do
              (update! screen :debug (not debug))
              entities)
            (= (:key screen) (key-code :g))
            (let [gunship (first (filter #(:gunship? %) entities))]
              (clojure.pprint/pprint gunship)))
      :paused
      (cond (= (:key screen) (key-code :p))
            (do
              (update! screen :game-state :in-game)
              entities)
            (= (:key screen) (key-code :d))
            (do
              (update! screen :debug (not debug))
              entities))))
      ;entities))

  :on-timer
  (fn [screen entities]
    (case (:id screen)
      :refresh-cannon-shot (do
                             (update! screen :fire-cannon-when-ready true)
                             entities)
      :refresh-gatling-shot (do
                              (update! screen :fire-gatling-when-ready true)
                              entities)
      :refresh-rocket-shot (do
                             (update! screen :fire-rocket-when-ready true)
                             entities))))

(defgame the-iron-council-game
  :on-create
  (fn [this]
    (set-screen! this main-screen hud/hud-screen)
    (graphics! :set-v-sync true)))

(-> main-screen :entities deref)
