(ns clj-gameoflife.life-test
  (:require [clojure.test :refer :all]
            [clj-gameoflife.life :refer :all]))            

(deftest get-cell-test
  (let [world1 (->World 1 1 [[1]])
        world0 (->World 1 1 [[0]])
        world-mod (->World 2 2 [[0 1] [2 3]])]
    (is (= 1 (get-cell world1 [0 0])))
    (is (= 0 (get-cell world0 [0 0])))
    (is (= 1 (get-cell world-mod [0 -1])))
    (is (= 2 (get-cell world-mod [1 2])))
    (is (= 3 (get-cell world-mod [-1 3])))))

(deftest new-world-test
  (let [world (new-world 3 5)
        cells (:cells world)]
    (is (= (count (first cells)) 5))
    (is (= (count cells) 3))))

(deftest set-cell-test
  (let [world (new-world 1 1)]
    (is (= 0 (get-cell world [0 0])))
    (is (= 1 (get-cell (set-cell world [0 0]) [0 0])))
    (is (= 1 (get-cell (set-cell world [-1 -1]) [0 0])))))

(deftest clear-cells-test
  (let [world (set-cell (new-world 1 2) [0 1])]
    (is (= 1 (get-cell world [0 1])))
    (is (= 0 (get-cell (clear-cells world) [0 1])))))

(deftest step-cell-test
  (let [world1 (->World 3 3 [[0 0 1] [1 0 1] [0 0 0]])
        world2 (->World 3 3 [[0 0 0] [1 0 1] [0 0 0]])
        world3 (->World 3 3 [[0 0 0] [1 1 1] [0 0 0]])
        world4 (->World 3 3 [[0 0 1] [1 1 1] [0 0 0]])
        world5 (->World 3 3 [[0 0 0] [0 1 1] [0 0 0]])
        world6 (->World 3 3 [[0 0 1] [1 1 1] [1 0 0]])]
    (is (= 1 (step-cell world1 [1 1]))) ;; cell is dead and has 3 neighbors
    (is (= 0 (step-cell world2 [1 1]))) ;; cell is dead and has 2 neighbors
    (is (= 1 (step-cell world3 [1 1]))) ;; cell is alive and has 2 neighbors
    (is (= 1 (step-cell world4 [1 1]))) ;; cell is alive and has 3 neighbors
    (is (= 0 (step-cell world5 [1 1]))) ;; cell is alive and has 1 neighbor
    (is (= 0 (step-cell world6 [1 1]))))) ;; cell is alive and has 4 neighbors

(deftest step-world-test
  (let [cells1 [[0 0 0 0 0] [0 0 0 0 0] [0 1 1 1 0] [0 0 0 0 0] [0 0 0 0 0]]
        cells2 [[0 0 0 0 0] [0 0 1 0 0] [0 0 1 0 0] [0 0 1 0 0] [0 0 0 0 0]]
        world (->World 5 5 cells1)]
    (is (= cells2 (:cells (step-world world))))
    (is (= cells1 (:cells (step-world (step-world world)))))))
