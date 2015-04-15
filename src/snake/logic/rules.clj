(ns snake.logic.rules)

;helper funtions
(defn v+ [v1 v2]
  (mapv + v1 v2))
(defn place-food? [] (< (rand) 0.05))

(def rev-time (atom false))
(def history (atom '()))
;snake
(def snake-body (atom {:body [[20 5] [20 4] [20 3] [20 2]], :dir [0 1], :dead? true}))

;functions that operate on snake
(defn self-collide? []
  (let [{:keys [body]} @snake-body]
    (some #{(first body)} (rest body))))

(defn grow-snake [body]
  (vec (conj body (nth body (dec (count body))))))

(defn snake-outside-screen? [block-amount]
  (let [{:keys [body]} @snake-body
        [x y] (first body)]
    (or (>= x block-amount) (< x 0)
        (>= y block-amount) (< y 0))))

(defn move-snake []
  (let [{:keys [body dir]} @snake-body]
    (let [new-head (v+ (first body) dir)]
      (swap! snake-body update-in [:body]
             #(vec (cons new-head (pop %))))                ;pop tail, add new head
      (swap! history (fn [x] (cons @snake-body x)))
      )))

(defn snake-change-dir [x y]
  (swap! snake-body update-in [:dir] (fn [_] reset! [x y])))


;food container
(def food (atom #{}))

(defn get-matrix [block-amount]
  (let [block-range (range 0 block-amount)
        matrix (apply concat (map #(zipmap block-range (repeat %)) block-range))]
    (vec matrix)))

(defn get-free-squares [block-amount]
  (let [all-squares (get-matrix block-amount)
        {:keys [body]} @snake-body]
    (filter #(nil? (get (set body) %)) all-squares)))

(defn get-food-pos [block-amount]
  (let [choices (get-free-squares block-amount)]
    (rand-nth choices)))

(defn reset-game! []
  (reset! snake-body {:body [[20 5] [20 4] [20 3] [20 2]], :dir [0 1], :dead? false})
  (reset! food #{}))

;watcher that detects food collision
;(add-watch snake-body :key (fn [k r os ns]
;                             (let [{:keys [body]} ns
;                                   meal  (get @food (first body))]
;                               (if-not  @rev-time
;                                 (when meal
;                                   (swap! food disj meal)                 ;Removes the food piece
;                                   (swap! snake-body update-in [:body]
;                                          #(grow-snake %)))))))            ;Re-adds tail, so it does not get shortened
;
(defn eat-grow []
  (let [{:keys [body]} @snake-body
        meal (get @food (first body))]
    (when meal
      (swap! food disj meal)
      (swap! snake-body update-in [:body] #(grow-snake %))            ;Re-adds tail, so it does not get shortened
      )))
; update
(defn update [block-amount]
  (let [{:keys [dead?]} @snake-body]
    (if (or (self-collide?) (snake-outside-screen? block-amount))
      (swap! snake-body update-in [:dead?] #(or true %)))
    (when-not (or dead? @rev-time)
        (when (place-food?)
          (swap! food conj (get-food-pos block-amount)))
        (eat-grow)
        (move-snake))
    (when (and @rev-time (not-empty @history))
      (let [old-state (first @history)]
      (reset! snake-body old-state)
      (swap! history (fn [x] (rest x)))))))