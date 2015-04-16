;;;; Contains the rules of a snake game.
(ns snake.logic.rules)

;;; Vector addition.
(defn v+ [v1 v2]
  (mapv + v1 v2))

;time reversal variables
(def rev-time (atom false))
(def history (atom '()))

;;; Snake contains all aspects of the game.
(def snake (atom {:body [[20 5] [20 4] [20 3] [20 2]], :dir [0 1], :dead? true :food #{}}))

;;; A vector of all cells in game. Not known before runtime.
(def block-matrix (atom []))

;;; Sets block-matrix.
(defn define-matrix [block-amount]
  (let [block-range (range 0 block-amount)
        matrix (apply concat (map #(zipmap block-range (repeat %)) block-range))]
    (reset! block-matrix (vec matrix))))

(defn get-free-squares []
  (let [{:keys [body]} @snake]
    (remove (set body) @block-matrix)))

;;; Functions that operate on snake.
(defn move-snake []
  (let [{:keys [body dir]} @snake
    new-head (v+ (first body) dir)]
      (swap! snake update-in [:body] #(->> %
                                         (pop)
                                         (cons new-head)
                                         (vec)))))

(defn grow-snake [body]
  ;; Readds the tail so there are duplicates.
  (->> body
       (count)
       (dec)
       (nth body)
       (conj body)))

(defn snake-change-dir [x y]
  (swap! snake update-in [:dir] (fn [_] reset! [x y])))

(defn eat-grow []
  (let [{:keys [body food]} @snake
        meal (get food (first body))]
    (when meal
      (swap! snake update-in [:food] disj meal)
      (swap! snake update-in [:body] grow-snake))))

(defn self-collide? []
  (let [{:keys [body]} @snake]
    (some #{(first body)} (rest body))))

(defn snake-outside-screen? [block-amount]
  (let [{:keys [body]} @snake
        [x y] (first body)]
    (or (neg? x) (>= x block-amount)
        (neg? y) (>= y block-amount))))

(defn place-food? []
  (let [{:keys [food]} @snake] (empty? food)))

;;; Add snake state to history every tick.
(defn record-state []
  (swap! history (fn [x] (cons @snake x))))

(defn reset-game! []
  (reset! snake {:body [[20 5] [20 4] [20 3] [20 2]], :dir [0 1], :dead? false, :food #{}})
  (reset! history '())
  (reset! rev-time false))

;;; Game loop.
(defn update [block-amount]
  (let [{:keys [dead?]} @snake]
    (if (or (self-collide?) (snake-outside-screen? block-amount))
      (swap! snake update-in [:dead?] #(or true %)))
    (when-not (or dead? @rev-time)
      (record-state)
      (when (place-food?)
        (swap! snake update-in [:food] conj (rand-nth (get-free-squares))))
      (eat-grow) (move-snake))
    (when (and @rev-time (not-empty @history))
      (let [old-state (first @history)]
        (reset! snake old-state)
        (swap! history rest))                                 ;pop history reverted to
      (when (empty? @history)
        (reset! rev-time false)
        (swap! snake update-in [:dead?] #(or true %))))))