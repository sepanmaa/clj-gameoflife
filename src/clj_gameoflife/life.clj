(ns clj-gameoflife.life)

(defrecord World [rows cols cells])

(defn make-cells
  "create a nested vector of values initialized to 0"
  [h w]
  (vec (repeat h (vec (repeat w 0)))))

(defn new-world
  "create a new world of given dimension"
  [rows cols]
  (->World rows cols (make-cells rows cols)))

(defn set-cells
  "replace the cells of a world with the vector given as a parameter"
  [world cells]
  (let [rows (:rows world)
        cols (:cols world)]
    (->World rows cols cells)))

(defn clear-cells
  "set the value of every cell in the world to 0"
  [world]
  (new-world (:rows world) (:cols world)))

(defn get-cell
  "return cell's value (0 or 1)"
  [world [row col]]
  (get-in (:cells world) [(mod row (:rows world))
                          (mod col (:cols world))]))

(defn set-cell
  "switch cell's state: dead cell comes to life, living cell dies
  0 -> 1
  1 -> 0"
  [world [row col]]
  (let [cells (:cells world)
        y (mod row (:rows world))
        x (mod col (:cols world))
        cell (get-cell world [row col])]
    (set-cells world (assoc-in cells [y x] (if (= cell 1) 0 1)))))

(def neighbors [[-1 -1] [-1 0] [-1 1] [1 -1] [0 -1] [0 1] [1 0] [1 1]])

(defn step-cell
  "set cell alive or dead (0 or 1) based on its neighbors according to the 
  rules of Conway's game of life"
  [world [row col]]
  (let [neighbor-cells (mapv #(mapv + [row col] %) neighbors)
        alive-neighbors (reduce + (map #(get-cell world %) neighbor-cells))
        alive (= 1 (get-cell world [row col]))]
    (cond
      (and alive (or (= alive-neighbors 2) (= alive-neighbors 3))) 1
      (and (not alive) (= alive-neighbors 3)) 1
      :else 0)))

(defn step-world
  "set a new value for every cell by calling step-cell"
  [world]
  (let [cells (vec (for [i (range (:rows world))]
                     (vec (for [j (range (:cols world))]
                            (step-cell world [i j])))))]
    (set-cells world cells)))        

(defn random-cells
  "randomly give every cell a value of 0 or 1"
  [world]
  (let [cells (mapv #(mapv (fn [x] (Math/round (Math/random))) %) (:cells world))]
    (set-cells world cells)))
