(ns sudoku.core
  (:require [clojure.set :as set]
            [reagent.core :as r]
            clojure.pprint))

(defn cells-by-row [row]
  (map #(-> {:row row :column %}) (range 9)))

(defn cells-by-column [column]
  (map #(-> {:row % :column column}) (range 9)))

(defn cells-by-box [box]
  (for [row (range 3) column (range 3)]
    {:row (+ row (* 3 (quot box 3)))
     :column (mod (+ column (* box 3)) 9)}))

(defn get-cells-from-board [board cells]
  (->> cells
       (map #(-> [% (get board %)]))
       (filter second)
       (into {})))

(defn has-duplicates [group]
  (let [counts (frequencies (vals group))]
    (->> group
         (filter
          (fn [[location value]]
            (> (get counts value) 1)))
         (map first))))

(defn validate-group [board selector]
  (->> (map selector (range 9))
       (map #(get-cells-from-board board %))
       (mapcat has-duplicates)
       set))

(defn validate [board]
  (set/union
   (validate-group board cells-by-row)
   (validate-group board cells-by-column)
   (validate-group board cells-by-box)))

(defn grid [n m component]
  [:table
   {:style
    {:width "100%"
     :height "100%"
     :border-collapse :collapse
     :border "2px solid #344861"}}
   [:tbody
    (for [n (range n)]
      [:tr
       {:key n :style {:margin 0 :padding 0}}
       (for [m (range m)]
         [:td
          {:key m :style {:margin 0 :padding 0}}
          (component n m)])])]])

(defn log [value] (println value) value)

(defn cell-input [a b errors state on-change]
  (fn [n m]
    (let [location {:row (+ n a) :column (+ m b)}
          error? (contains? errors location)]
      [:input
       {:value (get state location "")
        :on-change
        #(let [value (.-value (.-target %))]
           (if (empty? value)
             (on-change (dissoc state location))
             (let [value (js/parseInt value)]
               (when (and (not (js/isNaN value))
                          (< 0 value 10))
                 (on-change (assoc state location value))))))
        :style
        (merge
         {:display :block
          :width "100%"
          :height "100%"
          :text-align :center
          :color "#344861"
          :font-size "3rem"}
         (when error?
           {:background :pink
            :color :red
            :border "1px solid red"}))}])))

(defonce state (r/atom nil))

(defn app []
  (let [n 3
        on-change #(swap! state conj %)
        _state @state
        board (first _state)
        errors (validate board)]
    [:div
     {:style
      {:width "50vh"
       :height "50vh"}}
     ;[:div (pr-str state)]
     [:button
      {:disabled (empty? board)
       :on-click #(swap! state conj {})} "clear"]
     [:button
      {:disabled (empty? _state)
       :on-click #(swap! state rest)} "undo"]
     [grid n n
      #(let [a (* %1 n) b (* %2 n)]
         [grid n n (cell-input a b errors board on-change)])]]))

(defn main! []
  (r/render [app]
            (.getElementById js/document "root")))

(defn reload! [] (main!))

