;;;; Contains the rules of snake game.
(ns snake.logic.rules)
;;; Helper functions
(defn v+ [v1 v2]
  (mapv + v1 v2))

(defn abs [x]
  "Returns absolute value of x"
  (if (neg? x)
    (* -1 x)
    x))

;;; Time reversal variables.
(def reverse-time? (atom false))
(def game-history (atom '()))
(defn set-reverse-time! [p]
  "Sets reverse-time? to value p provided that rules are met."
    (if (empty? @game-history)
      (reset! reverse-time? false)
      (reset! reverse-time? p)))

(defn make-new-snake [dead?]
  "Creates and returns a map of all game variables, including food-pieces"
  (assoc {} :body (mapv vector (repeat 20) (range 5 1 -1)), ; A fair starting position for the snake
            :dir [0 1],
            :dead? dead?
            :turned? false                                  ; Prevents double turns on same game tick
            :food nil))                                     ; x,y location of food-piece.

;;; snake contains all aspects of the game except for the cell-matrix.
(def snake (atom (make-new-snake true)))

;;; Game board width counted in cells
(def width-in-cells 40)                                     ; You can set this yourself for higher 'resolution' board.
(defn get-width-in-cells [] width-in-cells)

;;; A vector of all cells in game. Acts as the game board.
(def cell-matrix (atom []))

(defn set-cell-matrix! []
  "Creates a two dimensional matrix of cells and sets the result
  to cell-matrix variable"
  (let [block-range (range 0 width-in-cells)
        matrix (apply concat (map #(zipmap block-range (repeat %)) block-range))]
    (reset! cell-matrix (vec matrix))))

(defn get-free-cells []
  "Returns a list of all unoccupied cells in the cell matrix"
  (let [{:keys [body]} @snake]
    (remove (set body) @cell-matrix)))

(defn move-snake []
  "Moves snake in the current direction. Shrinks tail (pop), grows head(cons)."
  (let [{:keys [body dir]} @snake
        new-head (v+ (first body) dir)]
      (swap! snake update-in [:body] #(->> %
                                         (pop)
                                         (cons new-head)
                                         (vec)))
      (swap! snake update-in [:turned?] #(and false %))))

(defn grow-snake []
  "Adds a duplicate of tail so when usual pop of tail
  happens the tail will remain, making the snake longer"
  (let [{:keys [body]} @snake
        new-body (conj body (last body))]
    (swap! snake update-in [:body] (fn [_] reset! new-body))))

(defn invalid-turn? [current-dir turn-dir]
  "Return true if turn-dir would turn more than 90 degrees."
  (let [[x y] current-dir
        [a b] turn-dir
        x-check (+ (abs x) (abs a))
        y-check (+ (abs y) (abs b))]
    (if (or (> x-check 1) (> y-check 1))                    ; x-check or y-check being > 1 means snake has turned 180 degrees.
      true
      false)))

(defn change-snake-dir! [turn-dir]
  "Changes the direction of snake."
  (let [{:keys [turned? dead? dir]} @snake]
    (when-not (or turned? dead? (invalid-turn? dir turn-dir))                                         ; If we have not turned this tick. We can change dir.
      (swap! snake update-in [:dir] (fn [_] reset! turn-dir))
      (swap! snake update-in [:turned?] #(or true %)))))

(defn consume-food []
  "Consumes the current food piece in :food making it empty."
  (swap! snake update-in [:food] (fn [_] reset! nil)))

(defn food-eaten? []
  "Returns true if there is currently no food-piece in any cell on the board."
  (let [{:keys [food]} @snake] (empty? food)))

(defn place-food! []
  "Creates a new food-piece on a randomly chosen unoccupied cell"
  (let [new-food  (vector (rand-nth (get-free-cells)))]     ; Not wrapping this in vector creates a bug. I dont want to wrap in vector.
    (swap! snake update-in [:food] (fn [_] reset! new-food))))

;;; Chose to abstract this because some bug forces food to be encapsuled in an extra vector for game to run.
(defn current-food []
  "Returns a vector of the current position of food piece"
  (let [{:keys [food]} @snake]
    (first food)))

(defn snake-found-food? []
  "If head of the position of snake's head and food piece is equal
  the function will return true, otherwise false is returned."
  (let [{:keys [body]} @snake
        snake-head (first body)
        collide? (= (current-food) snake-head)]
    (true? collide?)))

(defn snake-self-collide? []
  (let [{:keys [body]} @snake]
    (some #{(first body)} (rest body))))

(defn snake-outside-screen? []
  "Returns true if snake head is in an invalid position."
  (let [{:keys [body]} @snake
        [x y] (first body)]
    (or (neg? x) (>= x width-in-cells)
        (neg? y) (>= y width-in-cells))))

(defn update-dead! []
  "Checks if snake's head is outside of gameboard or colliding
  with its body and updates death status accordingly."
  (when (or
          (snake-self-collide?)
          (snake-outside-screen?))
    (swap! snake update-in [:dead?] #(or true %))))

(defn snake-is-dead? []
  (let [{:keys [dead?]} @snake] (true? dead?)))

(defn record-game-state! []
  "Adds current state of the game to game-history."
  (swap! game-history (fn [x] (cons @snake x))))

(defn rewind-state! []
  "Reverts game to previous game state."
  (if-not (empty? @game-history)
    (do
      (reset! snake (first @game-history))
      (swap! game-history rest))
    (do
      (set-reverse-time! false)
      (swap! snake update-in [:dead?] #(or true %)))))      ; No more time to rewind. Set snake to dead to pause game.

(defn reset-game! []
  (reset! snake (make-new-snake false))
  (reset! game-history '())
  (set-reverse-time! false))

;;; Game loop.
(defn game-tick []
  (update-dead!)
  (when-not (or @reverse-time? (snake-is-dead?))
    (record-game-state!)
    (move-snake)
    (when (snake-found-food?)
      (consume-food)
      (grow-snake))
    (when (food-eaten?)
      (place-food!)))                                       ; Places food on random free cell.
  (when @reverse-time?
    (rewind-state!)))