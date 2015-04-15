;Started off from tutorial: http://noobtuts.com/clojure/2d-snake-game
;so there are a lot of similarities in the code
(ns snake-game.core
  (:import [java.awt.event KeyEvent])
  (:use [quil.core :as q]))

;helper funtions
(defn v+ [v1 v2]
  (mapv + v1 v2))
(defn place-food? [] (< (rand) 0.05))

;screen variables
(def scr-size 400)
(def block-amount 40)
(def cell-width (/ scr-size block-amount))

;snake
(def snake-body (atom {:body [[20 5] [20 4] [20 3] [20 2]], :dir [0 1], :dead? true}))

;functions that operate on snake
(defn self-collide? []
  (let [{:keys [body]} @snake-body]
    (some #{(first body)} (rest body))))

(defn grow-snake [body]
  (vec (conj body (nth body (dec (count body))))))

(defn snake-outside-screen? []
  (let [{:keys [body]} @snake-body
        [x y] (first body)]
    (or (>= x block-amount) (< x 0)
        (>= y block-amount) (< y 0))))

(defn move-snake []
  (let [{:keys [body dir]} @snake-body]
    (let [new-head (v+ (first body) dir)]
      (swap! snake-body update-in [:body]
             #(vec (cons new-head (pop %))))))) ;pop tail, add new head

(defn snake-change-dir [x y]
  (swap! snake-body update-in [:dir] (fn [_] reset! [x y])))


;food container
(def food (atom #{}))

(defn get-matrix []
  (let [block-range (range 0 block-amount)
        matrix (apply concat (map #(zipmap block-range (repeat %)) block-range))]
    (vec matrix)))

(defn get-free-squares []
  (let [all-squares (get-matrix)
        {:keys [body]} @snake-body]
    (filter #(nil? (get (set body) %)) all-squares)))

(defn get-food-pos []
  (let [choices (get-free-squares)]
    (rand-nth choices)))

(defn reset-game! []
  (reset! snake-body {:body [[20 5] [20 4] [20 3] [20 2]], :dir [0 1], :dead? false})
  (reset! food #{}))
;watcher that detects food collision
(add-watch snake-body :key (fn [k r os ns]
                             (let [{:keys [body]} ns
                                  meal  (get @food (first body))]
                               (when meal
                                 (swap! food disj meal)                 ;Removes the food piece
                                   (swap! snake-body update-in [:body]
                                          #(grow-snake %))))))          ;Re-adds tail, so it does not get shortened

; update
(defn update []
  (let [{:keys [dead?]} @snake-body]
    (if (or (self-collide?) (snake-outside-screen?))
      (swap! snake-body update-in [:dead?] #(or true %)))
    (when-not dead?
      (when (place-food?) (swap! food conj (get-food-pos)))
      (move-snake))))

; draw
(defn draw []
  (let [{:keys [body dead?]} @snake-body]
    ; set background color to dark gray, draw color to white
    (q/background-float 0x20)
    ;draw snake
    (q/fill 0 255 0)
    (doseq [[x y] body
            green (range 0 (count body))]
      (q/rect (* x cell-width) (* y cell-width) cell-width cell-width))
    ;draw food
    (q/fill 255 0 0)
    (doseq [[x y] @food]
      (q/rect (* x cell-width) (* y cell-width) cell-width cell-width))
    (q/fill 255 255 255)
    (if dead?
      (q/text "Space to start!"
              (/ scr-size 2)
              (/ scr-size 2)))))

(defn setup []
  (q/smooth) (q/no-stroke) (q/frame-rate 30) (q/text-font (q/create-font "DejaVu Sans" 28 true))
  (q/text-align :center :center))
; input
(defn key-pressed []
  (let [{:keys [dir]} @snake-body]
    (cond
      (= (q/key-code) (KeyEvent/VK_UP)) (if (not (= dir [0 1])) (snake-change-dir 0 -1))
      (= (q/key-code) (KeyEvent/VK_DOWN)) (if (not (= dir [0 -1])) (snake-change-dir 0 1))
      (= (q/key-code) (KeyEvent/VK_LEFT)) (if (not (= dir [1 0])) (snake-change-dir -1 0))
      (= (q/key-code) (KeyEvent/VK_RIGHT)) (if (not (= dir [-1 0])) (snake-change-dir 1 0))
      (= (q/key-code) (KeyEvent/VK_SPACE)) (reset-game!)
      )))
(q/defsketch snake-game
             :title "Snake"
             :size [scr-size scr-size]
             :setup (fn []  (setup))
             :draw (fn [] (update) (draw))
             :key-pressed key-pressed)