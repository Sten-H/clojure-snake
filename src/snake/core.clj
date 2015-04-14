;Started off from tutorial: http://noobtuts.com/clojure/2d-snake-game
;so there are a lot of similarities in the code
(ns snake.core
  (:import [java.awt.event KeyEvent])
  (:use [quil.core :as q]))
;helper funtions
(defn v+ [v1 v2]
  (mapv + v1 v2))
(defn place-food? [] (< (rand) 0.1))

;food container
(def food (atom #{}))

;screen variables
(def scr-size 400)
(def block-amount 40)
(def cell-width (/ scr-size block-amount))


;snake
(def snake-body (atom [[0 0] [1 0] [2 0] [3 0]]))
(def snake-dir (atom [0 1]))
;functions that operate on snake
(defn self-collide? []
  (some #{(first @snake-body)} (rest @snake-body)))

(defn snake-outside-screen? []
  (let [[x y] (first @snake-body)]
    (or (>= x block-amount) (< x 0)
        (>= y block-amount) (< y 0))))

(defn move-snake []
  (let [new-head (v+ (first @snake-body) @snake-dir)]
           (swap! snake-body #(vec (cons new-head (pop %)))))       ;pop tail, add new head
  )
(defn reset-game! []
  (reset! snake-body [[0 0] [1 0] [2 0] [3 0]])
  (reset! food #{})
  (reset! snake-dir [0 1]))
;watcher that detects food collision
(add-watch snake-body :key (fn [k r os ns]
                        (let [meal  (get @food (first ns))]
                          (when meal
                            (swap! food disj meal)          ;Removes the food piece
                            (swap! snake-body
                                   #(vec (conj % (nth % (dec (count %)))))) ;Re-adds tail, so it does not get shortened
                            )))
           )
; update
(defn update []
  (when-not (or (self-collide?) (snake-outside-screen?))
    (when (place-food?) (swap! food conj [(rand-int block-amount) (rand-int block-amount)]))
    (move-snake))
  )

; draw
(defn draw []
  ; set background color to dark gray, draw color to white
  (q/background-float 0x20)
  ;draw snake
  (q/fill 0 255 0)
  (doseq [[x y] @snake-body]
    (q/rect (* x cell-width) (* y cell-width) cell-width cell-width))
  ;draw food
  (q/fill 255 0 0)
  (doseq [[x y] @food]
    (q/rect (* x cell-width) (* y cell-width) cell-width cell-width))
  ;(fill 0x00 0xff 0xff)
  ;(q/rect (* 1 cell-width) (* 1 cell-width) cell-width cell-width)
  )


; input
(defn key-pressed []
  (cond
    (= (q/key-code) (KeyEvent/VK_UP)) (reset! snake-dir [0 -1])
    (= (q/key-code) (KeyEvent/VK_DOWN)) (reset! snake-dir [0 1])
    (= (q/key-code) (KeyEvent/VK_LEFT)) (reset! snake-dir [-1 0])
    (= (q/key-code) (KeyEvent/VK_RIGHT)) (reset! snake-dir [1 0])
    (= (q/key-code) (KeyEvent/VK_SPACE)) (reset-game!)
    )
  )
(q/defsketch snake
             :title "Snake"
             :size [scr-size scr-size]
             :setup (fn [] (q/smooth) (q/no-stroke) (q/frame-rate 30))
             :draw (fn [] (update) (draw))
             :key-pressed key-pressed)