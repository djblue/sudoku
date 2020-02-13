(ns sudoku.core
  (:require [reagent.core :as r]))

(defn app []
  [:h1 "Hello, World!"])

(defn main! []
  (r/render [app]
            (.getElementById js/document "root")))

(defn reload! [] (main!))

