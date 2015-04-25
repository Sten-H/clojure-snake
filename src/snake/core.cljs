;;;; Draw functions of snake game.
(ns snake.core
  (:require [snake.logic.rules :as rules]
            [quil.core :as q :include-macros true]))

;screen variables
(def scr-size 400)
(def block-amount 40)
(def cell-width (/ scr-size block-amount))

(defn draw-text [dead?]
  (when dead?
    (q/fill 200 200 200)
    (q/text "Space to start! \nctrl to rewind time"
            (/ scr-size 2)
            (/ scr-size 2)))
  (when @rules/rev-time
    (q/fill 133 133 133)
    (q/text "Rewinding..."
            (/ scr-size 2)
            (/ scr-size 2))))

(defn draw []
  (let [{:keys [body dead? food]} @rules/snake]
    (q/background-float 0x20)
    ;draw snake
    (q/fill 0 255 0)
    (doseq [[x y] body]
      (q/rect (* x cell-width) (* y cell-width) cell-width cell-width))
    ;draw food
    (q/fill 255 0 0)
    (doseq [[x y] food]
      (q/rect (* x cell-width) (* y cell-width) cell-width cell-width))
    (draw-text dead?)))

(defn key-pressed []
  (let [{:keys [dir]} @rules/snake
        k (q/key-code)]
   (cond
     (= k 38) (if (not (= dir [0 1])) (rules/snake-change-dir 0 -1))
     (= k 40) (if (not (= dir [0 -1])) (rules/snake-change-dir 0 1))
     (= k 37) (if (not (= dir [1 0])) (rules/snake-change-dir -1 0))
     (= k 39) (if (not (= dir [-1 0])) (rules/snake-change-dir 1 0))
     (= k 17) (reset! rules/rev-time (not @rules/rev-time))
     (= k 32) (rules/reset-game!))))

(defn key-released []
      (let [k (q/key-code)]
           (when (= k 17) (reset! rules/rev-time (not @rules/rev-time)))))

(defn setup []
  (q/smooth) (q/no-stroke) (q/frame-rate 25) (q/text-font (q/create-font "DejaVu Sans" 28 true))
  (q/text-align :center :center) (rules/define-matrix block-amount))

(q/defsketch snake-game
             :host "snake-canvas"
             :size [scr-size scr-size]
             :setup (fn []  (setup))
             :draw (fn [] (rules/update block-amount) (draw))
             :key-pressed key-pressed
             :key-released key-released)