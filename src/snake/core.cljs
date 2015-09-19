;;;; Draw functions of snake game.
(ns snake.core
  (:require [snake.logic.rules :as rules]
            [quil.core :as q :include-macros true]))
;;; Screen variables
(def scr-size 400)
(def cell-width-in-pixels (atom nil))

(defn calculate-cell-width []
  "Calculates the pixel width of each cell for rendering."
  (reset! cell-width-in-pixels (/ scr-size (rules/get-width-in-cells))))


(defn draw-text [dead?]
  (when dead?
    (q/fill 200 200 200)
    (q/text "Space to start! \nctrl to rewind time"
            (/ scr-size 2)
            (/ scr-size 2)))
  (when @rules/reverse-time?
    (q/fill 133 133 133)
    (q/text "Rewinding..."
            (/ scr-size 2)
            (/ scr-size 2))))

(defn fill-cell [x y color]
  (let [[r g b] color]
    (q/fill r g b))
  (q/rect (* x @cell-width-in-pixels)
          (* y @cell-width-in-pixels)
          @cell-width-in-pixels
          @cell-width-in-pixels))

(defn draw-snake [body]
  (let [green [0 255 0]]
    (doseq [[x y] body]
      (fill-cell x y green))))

(defn draw-food [food]
  (let [[x y] (first food)
        red [255 0 0]]
    (fill-cell x y red)))

(defn draw []
  (let [{:keys [body dead? food]} @rules/snake]
    (q/background-float 0x20)
    (draw-snake body)
    (draw-food food)
    (draw-text dead?)))

(defn key-pressed []
  (let [k (q/key-as-keyword)]
      (cond
       (= k :up)            (rules/change-snake-dir! [0 -1])
       (= k :down)          (rules/change-snake-dir! [0 1])
       (= k :left)          (rules/change-snake-dir! [-1 0])
       (= k :right)         (rules/change-snake-dir! [1 0])
       (= k :control)       (rules/set-reverse-time! true)
       (= (q/key-code) 32)  (rules/reset-game!))))         ; This is spacebar press.

(defn key-released []
      (let [k (q/key-as-keyword)]
           (when (= k :control) (rules/set-reverse-time! false))))

(defn setup []
  (q/smooth) (q/no-stroke) (q/frame-rate 25) (q/text-font (q/create-font "DejaVu Sans" 28 true))
  (q/text-align :center :center) (q/fill [0 255 0])
  (calculate-cell-width)
  (rules/set-cell-matrix!))

(q/defsketch snake-game
             :host "snake-canvas"
             :size [scr-size scr-size]
             :setup (fn []  (setup))
             :draw (fn [] (rules/game-tick) (draw))
             :key-pressed key-pressed
             :key-released key-released)