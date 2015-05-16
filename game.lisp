(defpackage :game-of-life/game
  (:use :cl :game-of-life/engine :iterate)
  (:export #:game-of-life))
(in-package :game-of-life/game)

(defclass game-of-life (game)
  ((playingp :initarg :playing
             :initform t
             :accessor playingp))
  (:default-initargs
   :title "Game of life"
   :world (make-array '(100 100) :initial-element nil)
   :window-size '(800 800)
   :timestep 250))

(defmethod initialize-instance
    :after ((game game-of-life) &key (initial :pulsar) world
            &allow-other-keys)
  (flet ((place (px py array)
           (iter (for x from 0 below (array-dimension array 1))
             (iter (for y from 0 below (array-dimension array 0))
               (setf (aref world (+ x px) (+ y py))
                     (plusp (aref array y x)))))))
    (ecase initial
      (:blinker
       (place 1 2 #2a((1 1 1))))
      (:beacon
       (place 1 1 #2a((1 1 0 0)
                      (1 0 0 0)
                      (0 0 0 1)
                      (0 0 1 1))))
      (:toad
       (place 1 2 #2a((0 1 1 1)
                      (1 1 1 0))))
      (:glider
       (place 1 1 #2a((0 1 0)
                      (0 0 1)
                      (1 1 1))))
      (:pulsar
       (place 2 2 #2a((0 0 1 1 1 0 0 0 1 1 1 0 0)
                      (0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (1 0 0 0 0 1 0 1 0 0 0 0 1)
                      (1 0 0 0 0 1 0 1 0 0 0 0 1)
                      (1 0 0 0 0 1 0 1 0 0 0 0 1)
                      (0 0 1 1 1 0 0 0 1 1 1 0 0)
                      (0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 1 1 1 0 0 0 1 1 1 0 0)
                      (1 0 0 0 0 1 0 1 0 0 0 0 1)
                      (1 0 0 0 0 1 0 1 0 0 0 0 1)
                      (1 0 0 0 0 1 0 1 0 0 0 0 1)
                      (0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 1 1 1 0 0 0 1 1 1 0 0))))
      (:queen-bee-shuttle
       (place 1 1 #2a((0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 1 1)
                      (1 1 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 1)
                      (1 1 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0))))
      (:snark
       (place 1 1 #2a((0 0 0 0 0 0 1 1 0 0 0 1 1 0 0 0 0)
                      (0 0 0 0 0 0 1 1 0 0 1 0 1 1 1 0 0)
                      (0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1 0)
                      (0 0 0 0 0 0 1 1 1 1 0 1 1 0 0 1 0)
                      (0 0 0 0 0 0 1 0 0 1 0 1 0 1 0 1 1)
                      (0 0 0 0 0 0 0 0 0 1 0 1 0 1 0 1 0)
                      (0 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0)
                      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0)
                      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 1 0 0 0 0 0 0 0 1 1 0 0 0 0 0 0)
                      (0 1 0 1 0 0 0 0 0 1 1 0 0 0 0 0 0)
                      (0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 0 0)
                      (0 0 0 1 1 0 0 0 0 0 0 0 1 0 0 0 0)
                      (0 0 1 0 1 0 0 0 0 0 0 0 0 1 1 1 0)
                      (0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0))))
      (:5x5-infinite
       (place 50 50 #2a((1 1 1 0 1)
                        (1 0 0 0 0)
                        (0 0 0 1 1)
                        (0 1 1 0 1)
                        (1 0 1 0 1)))))))

(defun progress (world)
  (let* ((new-world (make-array (array-dimensions world) :initial-element 0)))
    (labels ((alivep (x y) (aref world x y))
             (count-neighbours (x y)
               (iter outer (for tx from (1- x) to (1+ x))
                 (iter (for ty from (1- y) to (1+ y))
                   (in outer (counting (and (array-in-bounds-p world tx ty)
                                            (not (and (= x tx) (= y ty)))
                                            (alivep tx ty))))))))
      (iter (for x from 0 below (array-dimension world 0))
        (iter (for y from 0 below (array-dimension world 1))
          (let ((neighbours (count-neighbours x y))
                (alivep (alivep x y)))
            (setf (aref new-world x y)
                  (if alivep
                      (cond
                        ((< neighbours 2) nil)
                        ((<= 2 neighbours 3) t)
                        ((> neighbours 3) nil))
                      (= neighbours 3)))))))
    new-world))

(defmethod tick ((game game-of-life) world events)
  (iter
    (for event = (pop events))
    (while event)
    (case (first event)
      (:key
       (case (second event)
         (:sdl-key-space
          (setf (playingp game) (not (playingp game))))))))

  (if (playingp game)
      (progress world)
      world))

(defmethod element-color ((game game-of-life) value)
  (if value
      '(255 255 255 1)
      '(0 0 0 1)))
