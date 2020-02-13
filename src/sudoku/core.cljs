(ns sudoku.core
  (:require [clojure.set :as set]
            [reagent.core :as r]
            [alandipert.storage-atom :refer [local-storage]]))

(def cells-by-row
  (memoize
   (fn cells-by-row [row]
     (map #(-> {:row row :column %}) (range 9)))))

(def cells-by-column
  (memoize
   (fn cells-by-column [column]
     (map #(-> {:row % :column column}) (range 9)))))

(def box-by-position
  (memoize
   (fn box-by-position [position]
     (let [{:keys [row column]} position]
       (+ (* (quot row 3) 3) (quot column 3))))))

(def cells-by-box
  (memoize
   (fn cells-by-box [box]
     (for [row (range 3) column (range 3)]
       {:row (+ row (* 3 (quot box 3)))
        :column (mod (+ column (* box 3)) 9)}))))

(def all-board-values
  (zipmap
   (for [row (range 9)
         column (range 9)]
     {:row row :column column})
   (repeat (set (map inc (range 9))))))

(def intersecting-cells
  (memoize
   (fn intersecting-cells [position]
     (concat
      (cells-by-row (:row position))
      (cells-by-column (:column position))
      (cells-by-box (box-by-position position))))))

(def cells
  (concat (map cells-by-row (range 9))
          (map cells-by-column (range 9))
          (map cells-by-box (range 9))))

(defn keep-duplicates [board-slice]
  (let [counts (frequencies (vals board-slice))]
    (keep
     (fn [[location value]]
       (when (> (get counts value) 1) location))
     board-slice)))

(defn get-errors [board]
  (set (mapcat #(keep-duplicates (select-keys board %)) cells)))

(defn remove-value [remaining-values [position value]]
  (reduce
   (fn [rv cell]
     (if-not (contains? rv cell)
       rv
       (update rv cell #(disj % value))))
   (dissoc remaining-values position)
   (intersecting-cells position)))

(defn solver
  ([board]
   (when (empty? (get-errors board))
     (solver board
             (reduce remove-value all-board-values board))))
  ([board remaining-values]
   (if (= (count board) (* 9 9))
     board
     (let [[k values]
           (first
            (sort-by
             (comp count second)
             remaining-values))]
       (some #(solver
               (assoc board k %)
               (remove-value remaining-values [k %])) values)))))

(defn generate-new-board []
  (into {}
        (random-sample
         0.45
         (solver
          (rand-nth
           [{{:row 8 :column 8} 8}
            {{:row 4 :column 4} 4}
            {{:row 3 :column 3} 3}
            {{:row 2 :column 2} 2}
            {{:row 1 :column 1} 1}
            {{:row 5 :column 5} 5}
            {{:row 0 :column 0} 1}])))))

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

(defn cell-input [props]
  (fn [n m]
    (let [{:keys [a b

                  puzzle
                  intersecting?
                  error?

                  board on-change
                  selected on-select

                  all-cells]} props
          location {:row (+ n a) :column (+ m b)}
          value (or (get puzzle location)
                    (get board location))
          counts (frequencies (vals all-cells))
          selected? (= value selected)
          done? (and selected? (= (get counts value) 9))]
      [:div
       {:style {:width "100%" :height "100%"}
        :on-click #(when value (on-select value))}
       [:input
        {:value value
         :disabled (contains? puzzle location)
         :on-change
         #(let [value (.-value (.-target %))]
            (if (empty? value)
              (on-change (dissoc board location))
              (let [value (js/parseInt value)]
                (when (and (not (js/isNaN value))
                           (< 0 value 10))
                  (on-change (assoc board location value))))))
         :style
         (merge
          {:display :block
           :width "100%"
           :height "100%"
           :text-align :center
           :color "#344861"
           :font-size "3rem"}
          (when selected?
            {:font-weight :bold})
          (when done?
            {:color :green})
          (when-not (or (intersecting? location)
                        (all-cells location))
            {:background :yellow})
          (when (error? location)
            {:background :pink
             :color :red
             :border "1px solid red"}))}]])))

(defonce state
  (local-storage
   (r/atom {:puzzle nil :input nil}) :app-state))

(defn app []
  (let [n 3

        on-select #(swap! state assoc :selected %)
        on-change #(swap! state update :input conj %)
        reset #(swap! state update :input conj {})
        undo #(swap! state update :input rest)
        new-game #(swap! state merge {:puzzle (generate-new-board)
                                      :input nil
                                      :selected nil})

        _state @state

        input (:input _state)
        selected (:selected _state)
        board (first input)
        puzzle (:puzzle _state)
        all-cells (merge {} puzzle board)
        error? (get-errors all-cells)
        completed?
        (set (keep (fn [[value count]]
                     (when (= count 9) value))
                   (frequencies (vals all-cells))))
        intersecting?
        (set (mapcat
              intersecting-cells
              (keep (fn [[location value]]
                      (when (= value selected)
                        location)) all-cells)))]
    [:div
     {:style
      {:width "80vh"
       :margin "0 auto"}}
     [:div
      {:style
       {:display :flex
        :align-items :center
        :justify-content :space-between}}
      [:h1 "Sudoku"]
      [:div
       [:button
        {:disabled (empty? input) :on-click reset} "clear"]
       [:button
        {:disabled (empty? input) :on-click undo} "undo"]
       [:button
        {:on-click new-game} "new game"]]]
     [:div
      {:style
       {:display :flex
        :position :relative}}
      [:div
       {:style
        {:width "60vh"
         :height "60vh"
         :margin-right 20}}
       (when (and
               (empty? error?)
               (= (count completed?) 9))
         [:div
          {:style
           {:position :absolute
            :top 0
            :bottom 0
            :left 0
            :right 0
            :color :white
            :font-weight :bold
            :font-size "6rem"
            :display :flex
            :align-items :center
            :justify-content :center
            :background "rgba(0,0,0,0.7)"}}
          "YOU WIN!"])
       [grid n n
        #(let [a (* %1 n) b (* %2 n)]
           [grid n n
            (cell-input
             {:a a
              :b b
              :error? error?
              :all-cells all-cells
              :intersecting? intersecting?
              :puzzle puzzle
              :board board
              :on-change on-change
              :selected selected
              :on-select on-select})])]]
      [:div
       {:style
        {:width "20vh"
         :height "20vh"}}
       [grid n n
        (fn [a b]
          (let [value (+ 1 b (* a n))]
            [:button
             {:on-click #(on-select value)
              :style
              (merge
               {:width "100%"
                :height "100%"
                :color "#344861"
                :font-size "3rem"}
               (when (completed? value)
                 {:color :green})
               (when (= value selected)
                 {:font-weight :bold}))} value]))]]]]))

(defn main! []
  (r/render [app]
            (.getElementById js/document "root")))

(defn reload! [] (main!))

