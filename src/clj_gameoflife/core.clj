(ns clj-gameoflife.core
  (:gen-class :main true)
  (:require [clj-gameoflife.life :refer :all])
  (:import
   (javax.swing JLabel JFrame JPanel JButton)
   (java.awt Image Graphics Color BorderLayout Dimension)
   (java.awt.event ActionListener ActionEvent MouseListener MouseEvent)))

(def running (atom true))
(def cellsize 10)

(def life (atom (new-world 60 80)))

(defn draw-world [world ^Graphics g]
  (doseq [row (range (:rows world))
          col (range (:cols world))]
    (if (= 1 (get-cell world [row col]))
      (.setColor g Color/RED)
      (.setColor g Color/WHITE))
    (.fillRect g (* col cellsize) (* row cellsize) (- cellsize 1) (- cellsize 1))))

(defn mouse-listener []
  (proxy [MouseListener] []
    (mouseEntered [e])
    (mouseExited [e])
    (mousePressed [e])
    (mouseReleased [e])
    (mouseClicked [^MouseEvent e]
      (let [x (int (/ (.getX e) cellsize))
            y (int (/ (.getY e) cellsize))]
        (swap! life set-cell [y x])))))
    
(defn make-panel []
  (let [panel (proxy [JPanel MouseListener] []
                (getPreferredSize []
                  (Dimension. 800 600))
                (paintComponent [^Graphics g]
                  (.setColor g Color/BLACK)
                  (.fillRect g 0 0
                             (* (:cols @life) cellsize)
                             (* (:rows @life) cellsize))
                  (draw-world @life g)))]  
    (doto ^JPanel panel
          (.setFocusable true)
          (.addMouseListener (mouse-listener)))
    panel))

(defn make-button [^String text fun]
  (let [button (JButton. text)]
    (doto ^JButton button
          (.setActionCommand text)
          (.addActionListener
           (proxy [ActionListener] []
             (actionPerformed [^ActionEvent e]
               (if (= (.getActionCommand e) text)
                 (fun button))))))))

(defn init-frame [^JFrame frame ^JPanel panel ^JPanel button-panel]
  (doto frame
    (.add panel BorderLayout/CENTER)
    (.add button-panel BorderLayout/PAGE_END)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.setVisible true)
    (.pack)))

(defn init-buttons [button-panel & buttons]
  (doseq [b buttons] (.add ^JPanel button-panel ^JButton b)))

(defn gui []
  (let [frame (JFrame. "Game of Life")
        simulation (atom false)
        button-panel (proxy [JPanel] [])
        quit (fn [b] (compare-and-set! running true false))
        randomize (fn [b] (swap! life (fn [w] (random-cells w))))
        clear (fn [b] (swap! life (fn [w] (clear-cells w))))
        simulate (fn [^JButton b]
                   (swap! simulation (fn [x] (do (if @simulation
                                                   (.setText b "simulate")
                                                   (.setText b "stop"))
                                                 (not x)))))
        step (fn [b] (swap! life (fn [w] (step-world w))))
        panel (make-panel)]
    (init-buttons button-panel
                  (make-button "step" step)
                  (make-button "quit" quit)
                  (make-button "simulate" simulate)
                  (make-button "clear" clear)
                  (make-button "randomize" randomize))
    (init-frame frame panel button-panel)
    (while @running
      (Thread/sleep 100)
      (if @simulation
        (step nil))
      (.repaint ^JPanel panel))
    (.dispose frame)))

(defn -main []
  (gui))
