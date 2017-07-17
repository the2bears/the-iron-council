(defproject the-iron-council "0.0.1-SNAPSHOT"
  :description "A lone gunship vs. The Iron Council"
  
  :dependencies [[com.badlogicgames.gdx/gdx "1.9.3"]
                 [com.badlogicgames.gdx/gdx-backend-lwjgl "1.9.3"]
                 [com.badlogicgames.gdx/gdx-box2d "1.9.3"]
                 [com.badlogicgames.gdx/gdx-box2d-platform "1.9.3"
                  :classifier "natives-desktop"]
                 [com.badlogicgames.gdx/gdx-platform "1.9.3"
                  :classifier "natives-desktop"]
                 [org.clojure/clojure "1.7.0"]
                 [play-clj "1.2.0-SNAPSHOT"]
                 [pixel-ships "0.1.0-SNAPSHOT"]]

  
  :source-paths ["src" "src-common"]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :aot [the-iron-council.core.desktop-launcher]
  :main the-iron-council.core.desktop-launcher)
