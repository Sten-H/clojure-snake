;Started off from tutorial: http://noobtuts.com/clojure/2d-snake-game
;so there are a lot of similarities in the code
(ns snake.core
  (:require [snake.logic.rules :as rules])
  (:import [java.awt.event KeyEvent])
  (:use [quil.core :as q]))
;screen variables
(def scr-size 400)
(def block-amount 40)
(def cell-width (/ scr-size block-amount))

(defn draw []
  (let [{:keys [body dead?]} @rules/snake-body]
    (q/background-float 0x20)
    ;draw snake
    (q/fill 0 255 0)
    (doseq [[x y] body]
      (q/rect (* x cell-width) (* y cell-width) cell-width cell-width))
    ;draw food
    (q/fill 255 0 0)
    (doseq [[x y] @rules/food]
      (q/rect (* x cell-width) (* y cell-width) cell-width cell-width))
    ;draw text if dead
    (q/fill 255 255 255)
    (if dead?
      (q/text "Space to start!"
              (/ scr-size 2)
              (/ scr-size 2)))))
; input
(defn key-pressed []
  (let [{:keys [dir]} @rules/snake-body]
    (cond
      (= (q/key-code) (KeyEvent/VK_UP)) (if (not (= dir [0 1])) (rules/snake-change-dir 0 -1))
      (= (q/key-code) (KeyEvent/VK_DOWN)) (if (not (= dir [0 -1])) (rules/snake-change-dir 0 1))
      (= (q/key-code) (KeyEvent/VK_LEFT)) (if (not (= dir [1 0])) (rules/snake-change-dir -1 0))
      (= (q/key-code) (KeyEvent/VK_RIGHT)) (if (not (= dir [-1 0])) (rules/snake-change-dir 1 0))
      (= (q/key-code) (KeyEvent/VK_SPACE)) (rules/reset-game!))))

(defn setup []                                              ;quil start setup
  (q/smooth) (q/no-stroke) (q/frame-rate 30) (q/text-font (q/create-font "DejaVu Sans" 28 true))
  (q/text-align :center :center))

(q/defsketch snake-game
             :title "Snake"
             :size [scr-size scr-size]
             :setup (fn []  (setup))
             :draw (fn [] (rules/update block-amount) (draw))
             :key-pressed key-pressed)