(ns tic-tac-toe.core
    (:require
      [reagent.core :as r]))

;; -------------------------
;; Model

(def start-board
  [[nil nil nil]
   [nil nil nil]
   [nil nil nil]])

(def init-db
  {:player 0
   :board start-board
   :game-state nil})

(defonce db (r/atom init-db))


;; -------------------------
;; Update

(defn toggle-player! [db]
  (swap! db update-in [:player] (fn [p] (case p 0 1 1 0))))

(defn set-color! [db x y color]
  (swap! db update-in [:board x y] (fn [_] color)))

(defn transpose [m] (apply mapv vector m))

(def diagonal [[0 0] [1 1] [2 2]])

;; b* is the "extended board"
(defn b* [board]
  (concat
   board
   (transpose board)
   ;; diagonals
   [(mapv (fn [ks] (get-in board ks)) diagonal)
    (mapv (fn [[x y]] (get-in board [x (- 2 y)])) diagonal)]))

(defn win? [player board]
  (let [f (fn [row] (every? #(= player %) row))]
    (some f (b* board))))

(defn winner? [board]
  (cond
    (win? 0 board) 0
    (win? 1 board) 1
    :else false))

(defn check-winner! [db]
  (if (winner? (:board @db)) ;; TODO: use return value?
    (swap! db update-in [:game-state] (fn [_] :over))
    (toggle-player! db)))

(defn empty-cell? [x y board]
  (= nil (get-in board [x y])))

(defn handle-select [db [x y]]
  (if (empty-cell? x y (:board @db))
    (do
      (set-color! db x y (:player @db))
      (check-winner! db))))

(defn dispatch! [[event arg]]
  (case event
    :select
    (if (:game-state @db)
      nil ;; ignore select action if game is already won
      (handle-select db arg))

    :reset
    (reset! db init-db)))


;; -------------------------
;; Views

(def player-0-color "gold")
(def player-1-color "pink")

(defn cell [cell-state on-click]
  (let [bg (case cell-state 0 player-0-color 1 player-1-color "grey")
        style {:display "inline-block"
               :height 40
               :width 40
               :background-color bg}]
    [:div {:style style :on-click on-click}]))

(defn row [[x y z] row-num]
  [:div
   [cell x #(dispatch! [:select [row-num 0]])]
   [cell y #(dispatch! [:select [row-num 1]])]
   [cell z #(dispatch! [:select [row-num 2]])]])

(defn board [[r0 r1 r2]]
  [:div
   [row r0 0]
   [row r1 1]
   [row r2 2]])

(defn home-page []
  [:div
   [:div
    [:div [:button {:on-click #(dispatch! [:reset nil])} "reset"]]
    [:span {:style {:background-color "gold"}} "player 0"]
    [:span {:style {:background-color "pink"}} "player 1"]]
   [board (:board @db)]
   (if (= :over (:game-state @db))
     [:div "Game over: player " (:player @db) " is the winner"])])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
