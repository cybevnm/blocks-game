;;;;
;;;; TODO: + current tetromino falling
;;;;       - levels
;;;;       + rows elimination
;;;;       ? rows elimination animation
;;;;       - tetromino spawning algorithm
;;;;       - tetromino sprites
;;;;       - GUI (well area, points, next tetrominos list)
;;;;       + tetromino dropping
;;;;       + current tetromino well bottom projection 
;;;;       - enhanced rotating system
;;;;       - building system, Makefile or something like this etc.
;;;;
;;;; defparameter - rebind value on each evaluation
;;;; defvar       - bind value only on first evaluation
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :blocks-game)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; constants

(defparameter *well-dimensions*  '(10 . 22))
(defparameter *piece-dimensions* '(15 . 15))
(defparameter *background-color*  sdl:*black*)
(defparameter *well-grid-color*   (sdl:color :r 128 :g 128 :b 128))
(defparameter *curr-frame-index*  0)

(defparameter *margin* 20)
(defparameter *stats-width* 50)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; common stuff

;;; actually should be called "partial" 
(defun curry (function &rest args)
  (lambda (&rest more-args)
    (apply function (append args more-args))))

;;; From http://www.cliki.net/COMPOSE
(defun compose (&rest functions)
  "Compose FUNCTIONS right-associatively, returning a function"
  (lambda (x)
    (reduce #'funcall functions
            :initial-value x
            :from-end t)))

(defun mapcar-until (mapping-func list test-func)
  "Returns part of the LIST mapped with MAPPING-FUNC until mapping result 
satisfies test-func"
  (if (null list)
      nil
      (let ((mapped-item (funcall mapping-func (car list))))
        (if (funcall test-func mapped-item)
            (cons mapped-item (mapcar-until mapping-func (cdr list) test-func))
            nil))))

(defun random-element (list)
  "Return some element of the list, chosen at random."
  (nth (random (length list)) list))

(defun in-range-p (value left right)
  (and (>= value left) (< value right)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; layout

(defun calc-window-size ()
  (cons (+ *margin*
           *stats-width*
           *margin*
           (* (car *well-dimensions*) (car *piece-dimensions*))
           *margin*)
        (+ *margin*
           (* (cdr *well-dimensions*) (cdr *piece-dimensions*))
           *margin*)))

(defun stats-window-pos ()
  (cons *margin* *margin*))

(defun well-window-pos ()
  (cons (+ *margin* *stats-width* *margin*) *margin*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; game stuff

;;; current game object
(defvar *game* nil)

;;; Entry point
(defun blocks-main ()
  (sdl:with-init ()
    (let ((*game* (make-instance 'blocks-game))
          (window-size (calc-window-size)))
      (sdl:window (car window-size) 
                  (cdr window-size)
                  :fps (make-instance 'sdl:fps-fixed
                                      :target-frame-rate 30))
      (sdl:enable-key-repeat 100 50)
                                        ;(setf (sdl:frame-rate) 30)
      (sdl:initialise-default-font sdl:*font-5x7*)
      (sdl:with-events (:poll)
        (:quit-event () t)
        (:video-expose-event () (sdl:update-display))
        (:key-down-event (:key key) (handle-input key))
        (:idle ()
               (sdl:clear-display *background-color*)
               (update-game)
               (draw-game)
               (sdl:update-display))))))

(defclass blocks-game ()
  ((well              :initform (make-instance 'well))
   (curr-tetromino    :initform nil)
   (single-clears-num :initform 0)
   (double-clears-num :initform 0)
   (triple-clears-num :initform 0)
   (tetrises-num      :initform 0)))

(defmethod initialize-instance :after ((game blocks-game) &key)
  (replace-curr-tetromino game (make-tetromino-of-type 'L 5 0)))

(defun replace-curr-tetromino (game tetromino)
  (setf (slot-value game 'curr-tetromino) tetromino))

(defun add-clear (game lines-cleared)
  (incf (slot-value game (ecase lines-cleared
                           (1 'single-clears-num)
                           (2 'double-clears-num)
                           (3 'triple-clears-num)
                           (4 'tetrises-num)))))

(defun spawn-next-tetromino ()
  (make-tetromino-of-type (random-element '(I J L O S T Z)) 5 -2))

(defun replace-well (game well)
  (setf (slot-value game 'well) well))

(defun handle-slime-requests ()
  (let ((connection
         (or swank::*emacs-connection* (swank::default-connection))))
                                        ;(when (and connection (not (eql swank:*communication-style* :spawn)))
                                        ;  (swank::handle-requests connection t))))
    (when connection
      (swank::handle-requests connection t))))

(defun handle-input (key)
  (with-slots (curr-tetromino well) *game*
    (multiple-value-bind (moving-type moving-remarks moved-tetromino)
        (handle-tetromino-keys key curr-tetromino well)
      (if (not (null moved-tetromino))
          (handle-moved-tetromino moving-type moving-remarks moved-tetromino well)
          (handle-common-keys key)))))

(defun handle-tetromino-keys (key tetromino well)
  (flet ((prepare-values (moving-type moving-result)
           "Temporary solution to prepare and return values from parent function"
           (values moving-type (car moving-result) (cdr moving-result))))
    (case key
      (:sdl-key-e     (prepare-values :rotating   (rotate-tetromino-cw  well tetromino)))
      (:sdl-key-w     (prepare-values :rotating   (rotate-tetromino-ccw well tetromino)))
      (:sdl-key-left  (prepare-values :horizontal (move-tetromino-left  well tetromino)))
      (:sdl-key-right (prepare-values :horizontal (move-tetromino-right well tetromino)))
      (:sdl-key-down  (prepare-values :vertical   (move-tetromino-down  well tetromino)))
      (:sdl-key-space (values :vertical :was-blocked (drop-tetromino well tetromino)))
      (otherwise nil))))

(defun handle-common-keys (key)
  (with-slots (well curr-tetromino) *game*
    (case key
      (:sdl-ley-q (sdl:push-quit-event)) ; don't work. why ?
      (:sdl-key-d (dump-well-to-stdout well))
      (:sdl-key-c (clear-well)))))

(defun update-game ()
  ;; REPL handling
  (handle-slime-requests)
  (with-slots (curr-tetromino well) *game*
    (let ((moving-result (fall-tetromino curr-tetromino well)))
      (handle-moved-tetromino :vertical (car moving-result) (cdr moving-result) well))))

(defparameter *falling-counter-threshold* 50)

(defun first-or-null (lst)
  (and lst (first lst)))

(defun fall-tetromino (tetromino well)
  (with-slots (x y center pieces falling-counter) tetromino
    (let ((new-falling-counter (1+ falling-counter)))
      (if (>= new-falling-counter *falling-counter-threshold*)
          (move-tetromino-down well tetromino)
          (cons :nothing-happened 
                (make-tetromino x y center pieces (1+ falling-counter)))))))

(defun update-game-objects (well tetromino)
  (let ((clearing-result (clear-lines well)))
    (replace-well *game* (cdr clearing-result))
    (replace-curr-tetromino *game* tetromino)
    (when (> (car clearing-result) 0)
      (add-clear *game* (car clearing-result)))))

(defun handle-moved-tetromino (moving-type moving-remarks moved-tetromino well)
  (ecase moving-remarks
    (:was-moved   (update-game-objects well moved-tetromino))
    (:was-blocked (when (eq moving-type :vertical) 
                    (if (tetromino-overflowed-well-p moved-tetromino)
                        (print "GAME OVER")
                        (update-game-objects (integrate-tetromino-to-well moved-tetromino well)
                                             (spawn-next-tetromino)))))
    (:nothing-happened (update-game-objects well moved-tetromino))))
  
(defun draw-piece (drawing-func piece &optional (well-x 0) (well-y 0))
  (with-slots (x y color) piece
    (when (and (in-range-p (+ well-x x) 0 (car *well-dimensions*))
               (in-range-p (+ well-y y) 0 (cdr *well-dimensions*)))
      (let ((well-xy (well->absolute-coordinates well-x well-y)))
        (funcall drawing-func 
                 (floor (+ (car well-xy) (* x (car *piece-dimensions*))))
                 (floor (+ (cdr well-xy) (* y (cdr *piece-dimensions*))))
                 (car *piece-dimensions*)
                 (cdr *piece-dimensions*)
               :color color)))))

(defun draw-filled-piece (piece &optional (well-x 0) (well-y 0))
  (draw-piece (lambda (x y w h &key color) 
                (sdl:draw-box-* x y (1- w) (1- h) :color color))
              piece 
              well-x
              well-y))

(defun draw-projected-piece (piece &optional (well-x 0) (well-y 0))
  (draw-piece #'sdl:draw-rectangle-* piece well-x well-y))

(defun draw-tetromino (drawing-func tetromino)
  (with-slots (x y color pieces) tetromino
    (dolist (piece pieces) (funcall drawing-func piece x y))))

(defun draw-filled-tetromino (tetromino)
  (draw-tetromino #'draw-filled-piece tetromino))

(defun draw-projected-tetromino (tetromino)
  (draw-tetromino #'draw-projected-piece tetromino))

(defun draw-game ()
  (with-slots (well curr-tetromino) *game*
    (draw-well well)
    (draw-filled-tetromino curr-tetromino)
    (draw-projected-tetromino (drop-tetromino well curr-tetromino))))

(defun draw-well-border (well well-window-pos)
  (sdl:draw-rectangle-* (1- (car well-window-pos))
                        (1- (cdr well-window-pos))
                        (+ (car (well-absolute-size well)) 2)
                        (+ (cdr (well-absolute-size well)) 2)))

(defun draw-well-pieces (well well-window-pos)
  (with-slots (pieces) well
    (let ((w (well-width well))
          (h (well-height well)))
      (dotimes (y h)
        (dotimes (x w)
          (unless (null (aref pieces (+ x (* y w))))
            (draw-filled-piece (aref pieces (+ x (* y w))) x y)))))))

(defun draw-well-grid (well)
  (let ((v-lines-num (1- (well-width  well)))
        (h-lines-num (1- (well-height well)))
        (well-pos (well-window-pos)))
    ;; vertical lines
    (dotimes (x v-lines-num)
      (sdl:draw-vline (+ (car well-pos) (* (car *piece-dimensions*) (1+ x)))
                      (cdr well-pos)
                      (1- (+ (cdr well-pos) (cdr (well-absolute-size well))))
                      :color *well-grid-color*))
    ;; horizontal lines
    (dotimes (y h-lines-num)
      (sdl:draw-hline (car well-pos)
                      (1- (+ (car well-pos) (car (well-absolute-size well))))
                      (+ (cdr well-pos) (* (cdr *piece-dimensions*) (1+ y)))
                      :color *well-grid-color*))))

(defun draw-well (well)
  (draw-well-pieces well (well-window-pos))
  (draw-well-grid   well)
  (draw-well-border well (well-window-pos)))

(defun dump-well-to-stdout (well)
  (with-slots (pieces) well
    (let ((w (well-width  well))
          (h (well-height well)))
      (dotimes (y h)
        (format t "~%")
        (dotimes (x w)
          (if (null (aref pieces (+ x (* y w))))
              (format t "~a" ".")
              (format t "~a" "x")))))
    (format t "~%")))

(defun clear-well ()
  (replace-well *game* (make-instance 'well)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; well stuff

(defclass well ()
  ((pieces :initform (make-array (* (car *well-dimensions*) 
                                    (cdr *well-dimensions*))
                                 :initial-element nil)	   
           :initarg :pieces))
  (:documentation "Well is main game area where tetrominos falls to bottom"))

(defun well-size (well)
  (declare (ignore well))
  *well-dimensions*)

(defun well-width (well)
  (car (well-size well)))

(defun well-height (well)
  (cdr (well-size well)))

(defun well-absolute-size (well)
  (cons (* (well-width well)  (car *piece-dimensions*))
        (* (well-height well) (cdr *piece-dimensions*))))

(defun piece-well-x (parent-tetromino piece)
  (with-slots ((parent-x x)) parent-tetromino
    (with-slots ((piece-x x)) piece
      (+ parent-x piece-x))))

(defun piece-well-y (parent-tetromino piece)
  (with-slots ((parent-y y)) parent-tetromino
    (with-slots ((piece-y y)) piece
      (+ parent-y piece-y))))

(defun well->absolute-coordinates (x y)
  (cons (+ (car (well-window-pos)) (* x (car *piece-dimensions*)))
        (+ (cdr (well-window-pos)) (* y (cdr *piece-dimensions*)))))

(defun piece-well-position (parent-tetromino piece)
  (cons (piece-well-x parent-tetromino piece)
        (piece-well-y parent-tetromino piece)))

(defun clone-well (old-well)
  (let ((new-well (make-instance 'well)))
    (with-slots ((new-pieces pieces)) new-well
      (with-slots ((old-pieces pieces)) old-well
        (let ((w (well-width  old-well))
              (h (well-height old-well)))
          (dotimes (y h)
            (dotimes (x w)
              (setf (aref new-pieces (+ x (* y w)))
                    (aref old-pieces (+ x (* y w)))))))))
    new-well))

(defun integrate-tetromino-to-well (tetromino well)
  (let ((new-well (clone-well well)))
    (with-slots ((well-pieces pieces)) new-well
      (with-slots ((tetromino-pieces pieces)) tetromino
        (dolist (piece tetromino-pieces)
          (with-slots ((rel-x x) (rel-y y)) piece
            (let ((abs-pos (piece-well-position tetromino piece)))
              (setf (aref well-pieces (+ (car abs-pos) 
                                         (* (cdr abs-pos) (well-width new-well))))
                    piece)
              (setf rel-x 0)
              (setf rel-y 0))))))
    new-well))

(defun filled-line-p (well row-index)
  (notany #'null (subseq (slot-value well 'pieces) 
                         (* row-index      (well-width well))
                         (* (1+ row-index) (well-width well)))))

(defun clear-line-n (well row-index)
  (with-slots (pieces) well
    (if (eql row-index 0)
        ;; clear first row
        (loop for x from 0 to (1- (well-width well))
           do (setf (aref pieces x) nil))
        ;; clear non-first row and ram all pieces above row-index
        (loop for y from row-index downto 1
           do (loop for x from 0 to (1- (well-width well))		 
                 do (setf (aref pieces (+ x (* y      (well-width well))))
                          (aref pieces (+ x (* (1- y) (well-width well))))))))))

(defun clear-lines (well)
  (let ((new-well (clone-well well))
        (filled-lines (loop for y from 0 to (1- (well-height well))
                         when (filled-line-p well y) 
                         collect y into lines and count y into lines-num
                         finally (return (cons lines lines-num)))))
    (mapc (curry #'clear-line-n new-well) (car filled-lines))
    (cons (cdr filled-lines) new-well)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; piece stuff

(defclass piece ()
  ((x     :initarg :x     :initform 0)
   (y     :initarg :y     :initform 0)
   (color :initarg :color :initform sdl:*white*))
  (:documentation "Primitive part of tetromino or well contents"))

(defun make-piece (x y color)
  (make-instance 'piece :x x :y y :color color))

(defun translate-piece (piece dx dy)
  (with-slots (x y color) piece
    (make-piece (+ x dx) (+ y dy) color)))

(defun translate-pieces (pieces dx dy)
  (mapcar (lambda (p) (translate-piece p dx dy)) pieces))

(defun transform-piece (piece m11 m12 m21 m22)
  (with-slots (x y color) piece
    (make-piece (+ (* x m11) (* y m21)) 
                (+ (* x m12) (* y m22)) 
                color)))

(defun transform-pieces (pieces m11 m12 m21 m22)
  (mapcar (lambda (p) (transform-piece p m11 m12 m21 m22)) pieces))

(defun floor-piece-coordinates (piece)
  (with-slots (x y color) piece
    (make-piece (floor x) (floor y) color)))

(defun floor-pieces-coordinates (pieces)
  (mapcar #'floor-piece-coordinates pieces))

(defun rotate-pieces (center pieces m11 m12 m21 m22)
  (let* ((center-x     (first center)) 
         (center-y     (second center))
         (translated   (translate-pieces pieces (- center-x) (- center-y)))
         (rotated      (transform-pieces translated m11 m12 m21 m22))
         (untranslated (translate-pieces rotated center-x center-y)))
    (floor-pieces-coordinates untranslated)))

(defun rotate-pieces-cw (center pieces)
  (rotate-pieces center pieces 0 1 -1 0))

(defun rotate-pieces-ccw (center pieces)
  (rotate-pieces center pieces 0 -1 1 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tetromino stuff

(defclass tetromino ()
  ((x               :initarg :x :initform 0
                    :documentation "X position of tetromino in well coordinates")
   (y               :initarg :y :initform 0
                    :documentation "Y position of tetromino in well coordinates")
   (center          :initarg :center 
                    :initform (error ":CENTER must not be NIL")
                    :documentation "Center point of tetromino relative to 
                		    tetromino pos")
   (pieces          :initarg :pieces 
                    :initform (error ":PIECES must not be NIL")
                    :documentation "List of child PIECEs")
   (falling-counter :initarg :falling-counter :initform 0)))

(defun make-tetromino (x y center pieces &optional (falling-counter 0))
  (make-instance 'tetromino 
                 :x x 
                 :y y 
                 :center center 
                 :pieces pieces
                 :falling-counter falling-counter))

(defparameter *orange* (sdl:color :r 255 :g 127 :b 0))
(defparameter *purple* (sdl:color :r 128 :g 0   :b 128))

(defun pieces-coords (type)
  (ecase type
    (I `(,sdl:*cyan*   (1.5 1.5) (0 1) (1 1) (2 1) (3 1)))
    (J `(,sdl:*blue*   (1.0 1.0) (0 0) (0 1) (1 1) (2 1)))
    (L `(,*orange*     (1.0 1.0) (2 0) (0 1) (1 1) (2 1)))
    (O `(,sdl:*yellow* (0.5 0.5) (0 0) (1 0) (0 1) (1 1)))
    (S `(,sdl:*green*  (1.0 1.0) (1 0) (2 0) (0 1) (1 1)))
    (T `(,*purple*     (1.0 1.0) (1 0) (0 1) (1 1) (2 1)))
    (Z `(,sdl:*red*    (1.0 1.0) (0 0) (1 0) (1 1) (2 1)))))

(defun color-from-coords (coords)
  (first coords))

(defun center-from-coords (coords)
  (second coords))

(defun pieces-from-coords (coords)
  (loop for xy in (cddr coords) 
     collect (make-piece (first xy) (second xy) (color-from-coords coords))))

(defun make-tetromino-of-type (type x y)
  (let ((coords (pieces-coords type)))
    (make-tetromino
     x
     y
     (center-from-coords coords)
     (pieces-from-coords coords))))

(defun reduce-tetromino-pieces (tetromino func key-func)
  (with-slots (pieces) tetromino
    (reduce func pieces :key key-func)))

(defun tetromino-left-border (tetromino)
  (reduce-tetromino-pieces tetromino #'min (curry 'piece-well-x tetromino)))

(defun tetromino-top-border (tetromino)
  (reduce-tetromino-pieces tetromino #'min (curry 'piece-well-y tetromino)))

(defun tetromino-right-border (tetromino)
  (reduce-tetromino-pieces tetromino #'max (curry 'piece-well-x tetromino)))

(defun tetromino-bottom-border (tetromino)
  (reduce-tetromino-pieces tetromino #'max (curry 'piece-well-y tetromino)))

(defun tetromino-borders (tetromino)
  (values (tetromino-left-border   tetromino)
          (tetromino-top-border    tetromino)
          (tetromino-right-border  tetromino)
          (tetromino-bottom-border tetromino)))

(defun tetromino-inside-of-well-p (well tetromino)
  "Returns true if tetromino don't overlapps well's borders"
  (multiple-value-bind (l-border t-border r-border b-border) 
      (tetromino-borders tetromino)
    (let ((well-width  (well-width well))
          (well-height (well-height well)))
      (and (>= l-border 0) (<  r-border well-width) (<  b-border well-height)))))

(defun point-inside-of-well-p (well x y)
  (let ((well-width  (well-width well))
        (well-height (well-height well)))
    (and (>= x 0) (< x well-width) (>= y 0) (< y well-height))))

(defun tetromino-overlaps-well-pieces-p (well tetromino)
  "Return true if tetromino clashes with pieces integrated in the well"
  (with-slots ((well-pieces pieces)) well
    (with-slots ((tetromino-pieces pieces)) tetromino
      (dolist (tetromino-piece tetromino-pieces)
        (let ((tetromino-piece-pos (piece-well-position tetromino tetromino-piece)))
          (when (point-inside-of-well-p well 
                                        (car tetromino-piece-pos) 
                                        (cdr tetromino-piece-pos))
            (when (not (null (aref well-pieces (+ (car tetromino-piece-pos) 
                                                  (* (cdr tetromino-piece-pos)
                                                     (well-width well))))))
              (return t))))))))

(defun tetromino-overflowed-well-p (tetromino)
  (< (tetromino-top-border tetromino) 0))

(defun tetromino-in-allowed-position-p (well tetromino)
  (and (tetromino-inside-of-well-p well tetromino)
       (not (tetromino-overlaps-well-pieces-p well tetromino))))

(defun move-tetromino (well tetromino dx dy &optional reset-falling-counter)
  (let* ((actual-dx (signum dx))	
         (actual-dy (signum dy))
         (moved-tetromino 
          (with-slots (x y center pieces falling-counter) tetromino
            (make-tetromino (+ x actual-dx) 
                            (+ y actual-dy) 
                            center
                            pieces
                            (if reset-falling-counter
                                0
nn                                falling-counter)))))
    (if (tetromino-in-allowed-position-p well moved-tetromino)
        (cons :was-moved moved-tetromino)
        (cons :was-blocked tetromino))))

(defun move-tetromino-left (well tetromino)
  (move-tetromino well tetromino -1 0))

;; (defun move-tetromino-up (well tetromino)
;;   (move-tetromino well tetromino 0 -1))

(defun move-tetromino-right (well tetromino)
  (move-tetromino well tetromino 1 0))

(defun move-tetromino-down (well tetromino)
  (move-tetromino well tetromino 0 1 t))

(defun drop-tetromino (well tetromino)
  (let ((moved-tetromino (move-tetromino-down well tetromino)))
    (if (eql (car moved-tetromino) :was-blocked)
        (cdr moved-tetromino)
        (drop-tetromino well (cdr moved-tetromino)))))

(defun rotate-tetromino (well tetromino direction)
  (let ((rotated-tetromino 
         (with-slots (x y center pieces falling-counter) tetromino
           (make-tetromino x 
                           y
                           center 
                           (if (< direction 0) 
                               (rotate-pieces-ccw center pieces)
                               (rotate-pieces-cw  center pieces))
                           falling-counter))))
    (if (tetromino-in-allowed-position-p well rotated-tetromino)
        (cons :was-moved   rotated-tetromino)
        (cons :was-blocked tetromino))))

(defun rotate-tetromino-ccw (well tetromino)
  (rotate-tetromino well tetromino -1))

(defun rotate-tetromino-cw (well tetromino)
  (rotate-tetromino well tetromino 1))
