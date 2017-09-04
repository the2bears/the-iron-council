(ns the-iron-council.debug-renderer
  (:require [play-clj.core :refer :all]
            [play-clj.math :refer :all])
  (:import [com.badlogic.gdx.graphics.glutils ShapeRenderer]))

(defn- render-collider [^ShapeRenderer shape-renderer
                        {:keys [collider collider-type] :as entity}]
  (case collider-type
    :poly (doto shape-renderer
            (.begin)
            (.polygon (polygon! collider :get-vertices))
            (.end))
    :rect (doto shape-renderer
            (.begin)
            (.rect (rectangle! collider :get-x)
                   (rectangle! collider :get-y)
                   (rectangle! collider :get-width)
                   (rectangle! collider :get-height))
            (.end))
    "default"))

(defn- render-collider2 [^ShapeRenderer shape-renderer
                         {:keys [collider-poly collider-type] :as entity}]
  (case collider-type
    :test (doto shape-renderer
            (.begin)
            (.polygon (polygon! collider-poly :get-vertices))
            (.end))
    "default"))

(defn render [{:keys [camera shape-renderer] :as screen} entities]
  (doto shape-renderer
    (.setProjectionMatrix (.combined camera))
    (.setColor 1 1 1 1)
    (.setAutoShapeType true))  
  (if-let [collidables (filter #(some? (:collider %)) entities)]
    (run! (fn[collidable]
            (render-collider shape-renderer collidable))
          collidables))    
  entities)
