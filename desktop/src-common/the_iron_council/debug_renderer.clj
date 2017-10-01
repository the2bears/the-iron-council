(ns the-iron-council.debug-renderer
  (:require [play-clj.core :refer :all]
            [play-clj.math :refer :all])
  (:import [com.badlogic.gdx.graphics.glutils ShapeRenderer]))

(defn- render-collider [^ShapeRenderer shape-renderer
                        {:keys [collider collider-type] :as entity}]
  (case collider-type
    :poly (doto shape-renderer
            (.setColor 1 0 0 1)
            (.begin)
            (.polygon (polygon! collider :get-vertices))
            (.end))
    :multi (doseq [enemy collider]
             (render-collider shape-renderer enemy))
    :rect (doto shape-renderer
            (.setColor 1 0 0 1)
            (.begin)
            (.rect (rectangle! collider :get-x)
                   (rectangle! collider :get-y)
                   (rectangle! collider :get-width)
                   (rectangle! collider :get-height))
            (.end))
    :circle (doto shape-renderer
              (.setColor 0 0 1 1)
              (.begin)
              (.circle (.x collider)
                       (.y collider)
                       (.radius collider)
                       8)
              (.end))
    "default"))

(defn render [{:keys [camera shape-renderer] :as screen} entities]
  (doto shape-renderer
    (.setProjectionMatrix (.combined camera))
    (.setAutoShapeType true))  
  (if-let [collidables (filter #(some? (:collider %)) entities)]
    (run! (fn[collidable]
            (render-collider shape-renderer collidable))
          collidables))    
  entities)
