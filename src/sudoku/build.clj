(ns sudoku.build
  (:require [clojure.java.io :as io]
            [shadow.cljs.devtools.api :as shadow]))

(defn -main []
  (binding [*out* *err*] (shadow/release :app))
  (println
   (str
    "<div id=\"root\"></div>"
    "<script>"
    (slurp "target/main.js")
    "</script>")))

