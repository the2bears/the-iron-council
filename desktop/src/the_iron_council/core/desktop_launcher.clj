(ns the-iron-council.core.desktop-launcher
  (:require [the-iron-council.core :refer :all])
  (:import [com.badlogic.gdx.backends.lwjgl LwjglApplication]
           [org.lwjgl.input Keyboard])
  (:gen-class))

(defn -main
  []
  (LwjglApplication. the-iron-council-game "the-iron-council" (* 3 224) (* 3 288))
  (Keyboard/enableRepeatEvents true))
