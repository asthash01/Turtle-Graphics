(ns assignment3.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

;-----------------------------------------------------------------------------------------------------------

;This Function read the commands from the "Commands.dat" file
;and Returns a vector that contains all the given commands in file.
(defn read-file-commands
      [p]
    (let [e (let [r (clojure.java.io/reader p)]
                 (for [a (line-seq r)]
                      (if-not (= a nil)
                      a)))]
            (into [] (for [x (map #(clojure.string/split % #"\ ") e)]
                                   {:action (nth x 0) :value (nth x 1 0)}))))

;Defining an atom 'commands' that contains vector of the commands read from the file.
(def commands (atom (read-file-commands "Commands.dat")))
(deref commands)

;Defining an atom 'message'
(def message (atom "No keyboard"))

;-----------------------------------------------------------------------------------------------------------

;Defining an atom 'previous-position'
(def previous-position (atom []))

;This Function will called when the user pressed the :right key,
;and add the current state of the turtle to 'previous-position'
(defn add-state
      [state]
      (swap! previous-position conj state))

;This Function will be called when the user pressed the :left key,
;and pop the last/previous state of the turtle from the 'previous-position'
(defn previous-state
      []
    (let [last-state (last @previous-position)]
         (if-not (empty? @previous-position)
                 (swap! previous-position pop))
                   last-state))

;-----------------------------------------------------------------------------------------------------------

;Defining an atom 'position'
(def position (atom 0))

;This Function will be called when user presses the keybord :left, :right, and :r keys.
(defn keyboard-action
      []
  (let [key (q/key-as-keyword)]
            (when (= key :right)               ;When :right key is pressed
                  (when (< @position 0)
                        (reset! position 0))
                  (reset! message (str (nth @commands @position)))
                  (add-state (nth @commands @position))
                  (swap! position inc))
            (when (= key :left)                 ;When :left key is pressed
                  (if (= @position 0)
                      (reset! message "No More Commands")
                      (reset! message (str (nth @commands (- @position 1) nil))))
                  (if-not (= position 0)
                          (do
                             (swap! position dec)
                             (previous-state))))
            (when (= key :r)                     ;When :r key is pressed
                  (reset! message "Run Mode")
                  (reset! position 0)
                  (reset! previous-position [])
                  (doseq [c @commands]
                         (add-state c)))))

;-----------------------------------------------------------------------------------------------------------

;Defining an atom 'pen-up'
(def pen-up (atom 0))

;This function will move the turtle as per the given commands, in step mode
(defn step-movement
      []
    (doseq [y @previous-position]
           (let [p (:action y) x (read-string (:value y))]
                (when (= p "[Move]")        ;Move the turtle by value x
                      (q/line 0 0 x 0)
                      (q/translate x 0))
                (when (= p "[Turn]")        ;turn the turtle by x radians
                      (q/rotate (q/radians x)))
                (when (= p "[PenUp]")       ;Move the turtle by value x but nothing is drawn
                      (q/stroke 240)
                      (reset! pen-up 1))
                (when (= p "[PenDown]")     ;Move the turtle by value x, a line is drawn is drawn
                      (q/stroke 0 255 0)))))

;-----------------------------------------------------------------------------------------------------------

;This function will be Called once
;Setup and initialize state
(defn setup
      []
    (q/frame-rate 2)                  ;Set frame rate to 2 frames per second.
    (q/text-size 20))                 ;set the text size to 20


;This function will be called every time and draw the sketch
(defn draw-state
      []
    (q/background 240)                ;set the background color to 240
    (q/fill 255 0 0)                  ;set the color of text and turtle to red
    (q/stroke 0  0 255)
    (q/text @message 30 30)           ;displays the text message on the window
    (q/translate 250 250)
    (step-movement)
    (if (= pen-up 0)
        (q/ellipse 0 0 10 10)
    (do
        (q/stroke 255 0 0)
        (q/ellipse 0 0 10 10)
        (q/stroke 240))))


;This function Defines and starts a sketch
(q/defsketch quil-test
             :title "My Turtle Graphics"         ;Set the title on the window screen
             :size [400 500]                     ;set the window screen size
             :setup setup
             :draw draw-state
             :key-pressed keyboard-action
             )

;End of the program
;-----------------------------------------------------------------------------------------------------------
