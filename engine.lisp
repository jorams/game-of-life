(defpackage :game-of-life/engine
  (:use :cl :iterate)
  (:export #:game
           #:tick
           #:render
           #:element-color
           #:start))
(in-package :game-of-life/engine)

(defvar *game* nil)

;;; API

(defclass game ()
  ((timestep :initarg :timestep
             :initform 16
             :accessor timestep)
   (world :initarg :world
          :accessor world)
   (background-color :initarg :background-color
                     :initform '(0 0 0 0)
                     :accessor background-color)
   (title :initarg :title
          :initform "Game"
          :accessor title)
   (window-size :initarg :window-size
                :initform '(800 480)
                :accessor window-size)
   (%events :initarg :%events
            :initform ()
            :accessor %events)))

(defgeneric tick (game world events)
  (:method :around (game world events)
    (declare (ignorable game world events))
    (when (next-method-p)
      (setf (world game) (call-next-method)))))
(defgeneric render (game world))
(defgeneric element-color (game value))

;;; Utilities

(defmacro with-game ((&optional (game 'game) &rest initargs) &body body)
  `(let ((*game* (apply #'make-instance ,game ,initargs)))
     ,@body))

;;; SDL-specific

(defun start (game-type)
  (sdl:with-init (sdl:sdl-init-video)
    (with-game (game-type)
      (initialize-display *game*)
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event
         (:key key :mod-key mod)
         (handle-key *game* key mod :down))
        (:key-up-event
         (:key key :mod-key mod)
         (handle-key *game* key mod :up))
        (:idle () (render *game* (world *game*)))))))

(defun handle-key (game key mod type)
  (when (eq type :down)
    (case key
      (:sdl-key-q (sdl:push-quit-event))
      (t (push (list :key key :mod mod :type type)
               (%events game))))))

(defun update (ticks dt)
  (declare (ignore ticks dt))
  (tick *game* (world *game*) (%events *game*))
  (setf (%events *game*) ()))

(defun initialize-display (game)
  (let* ((window-size (window-size *game*))
         (width (first window-size))
         (height (second window-size)))
    (sdl:window width height
                :fps (make-instance 'sdl:fps-unlocked
                                    :dt (timestep *game*)
                                    :ps-fn 'update)
                :title-caption (title game)
                :opengl t
                :opengl-attributes '((:sdl-gl-doublebuffer 1)))
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 (array-dimension (world *game*) 0)
              (array-dimension (world *game*) 1) 0
              -1 1)))

(defmethod render :around (game world)
  (declare (ignorable world))
  (apply #'gl:clear-color (background-color game))
  (gl:clear :color-buffer-bit)
  (when (next-method-p)
    (call-next-method))
  (gl:flush)
  (sdl:update-display))

(defmethod render (game world)
  (flet ((draw-pixel (x y primitive)
           (gl:with-primitive primitive
             (gl:vertex x y)
             (gl:vertex x (1+ y))
             (gl:vertex (1+ x) (1+ y))
             (gl:vertex (1+ x) y))))
    (iter (for x from 0 below (array-dimension world 0))
      (iter (for y from 0 below (array-dimension world 1))
        (apply #'gl:color (element-color game (aref world x y)))
        (draw-pixel x y :quads)))))
