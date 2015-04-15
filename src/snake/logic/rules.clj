(ns snake.logic.rules)

;helper funtions
(defn v+ [v1 v2]
  (mapv + v1 v2))

;
(def block-matrix (atom []))
;time reversal
(def rev-time (atom false))
(def history (atom '()))
;snake
(def snake (atom {:body [[20 5] [20 4] [20 3] [20 2]], :dir [0 1], :dead? true :food #{}}))
(swap! history (fn [x] (cons @snake x)))

;functions that operate on snake
(defn place-food? []
  (let [{:keys [food]} @snake] (empty? food)))

(defn self-collide? []
  (let [{:keys [body]} @snake]
    (some #{(first body)} (rest body))))

(defn grow-snake [body]
  (vec (conj body (nth body (dec (count body))))))

(defn snake-outside-screen? [block-amount]
  (let [{:keys [body]} @snake
        [x y] (first body)]
    (or (>= x block-amount) (< x 0)
        (>= y block-amount) (< y 0))))

(defn move-snake []
  (let [{:keys [body dir]} @snake]
    (let [new-head (v+ (first body) dir)]
      (swap! snake update-in [:body]
             #(vec (cons new-head (pop %)))))))                ;pop tail, add new head


(defn snake-change-dir [x y]
  (swap! snake update-in [:dir] (fn [_] reset! [x y])))

(defn record-state []
  (swap! history (fn [x] (cons @snake x))))

(defn define-matrix [block-amount]
  (let [block-range (range 0 block-amount)
        matrix (apply concat (map #(zipmap block-range (repeat %)) block-range))]
    (reset! block-matrix (vec matrix))))

(defn get-free-squares []
  (let [{:keys [body]} @snake]
    (filter #(nil? (get (set body) %)) @block-matrix)))

(defn get-food-pos []
  (let [choices (get-free-squares)]
    (rand-nth choices)))

(defn reset-game! []
  (reset! snake {:body [[20 5] [20 4] [20 3] [20 2]], :dir [0 1], :dead? false, :food #{}})
  (reset! history '())
  (reset! rev-time false))

(defn eat-grow []
  (let [{:keys [body food]} @snake
        meal (get food (first body))]
    (when meal
      (swap! snake update-in [:food] disj meal)
      (swap! snake update-in [:body] #(grow-snake %))            ;Re-adds tail, so it does not get shortened
      )))
; update
(defn update [block-amount]
  (let [{:keys [dead?]} @snake]
    (if (or (self-collide?) (snake-outside-screen? block-amount))
      (swap! snake update-in [:dead?] #(or true %)))
    (when-not (or dead? @rev-time)
      (record-state)
      (when (place-food?)
        (swap! snake update-in [:food] conj (get-food-pos)))
      (eat-grow)
      (move-snake))
    (when (and @rev-time (not-empty @history))
      (let [old-state (first @history)]
      (reset! snake old-state)
      (swap! history (fn [x] (rest x)))
      (when (empty? @history)
        (reset! rev-time false)
        (swap! snake update-in [:dead?] #(or true %)))))))