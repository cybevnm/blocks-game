;;;; Simple tetris-like game written in common lisp
;;;; Copyright (C) 2013 cybevnm

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :blocks-game
  (:use :common-lisp))

(in-package :blocks-game)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; constants

(defparameter *well-dimensions*  '(10 . 22))
(defparameter *piece-dimensions* '(15 . 15))
(defparameter *background-color*  sdl:*black*)
(defparameter *well-grid-color*   (sdl:color :r 128 :g 128 :b 128))

(defparameter *margin* 20)
(defparameter *stats-width* 70)

(defparameter *orange* (sdl:color :r 255 :g 127 :b 0))
(defparameter *purple* (sdl:color :r 128 :g 0   :b 128))
(defparameter *light-blue* (sdl:color :r 50 :g 50 :b 255))

(defparameter *levels-falling-thresholds* '(50 40  30  20   10   5    3))
(defparameter *levels-scores*             '(0  250 500 1000 2000 4000 8000))

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

(defmacro dolist-counting ((var index list) &rest body)
  "Complete dolist analog but also maintains current item index"
  `(let ((,index 0))
     (dolist (,var ,list)       
       ,@body
       (incf ,index))))

(defun random-list-permutation (list)
  "O(n^2) random list permutation implementation"
  (when list
    (let* ((list-len (length list))
           (elmt-ind (random list-len)))
      (cons (nth elmt-ind list)
            (random-list-permutation (append (butlast list (- list-len elmt-ind))
                                             (nthcdr (1+ elmt-ind) list)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; well stuff

(defclass well ()
  ((pieces :initform (make-array (* (car *well-dimensions*) 
                                    (cdr *well-dimensions*))
                                 :initial-element nil)
           :initarg :pieces))
  (:documentation "Well is main game area where tetrominos falls to bottom"))

(defun well-width ()
  (car *well-dimensions*))

(defun well-height ()
  (cdr *well-dimensions*))

(defun well-pixels-size ()
  (cons (* (well-width)  (car *piece-dimensions*))
        (* (well-height) (cdr *piece-dimensions*))))

(defun piece-well-position (parent-tetromino piece)
  (with-slots ((parent-x x) (parent-y y)) parent-tetromino
    (with-slots ((piece-x x) (piece-y y)) piece
      (cons (+ parent-x piece-x) (+ parent-y piece-y)))))

(defun clone-well (old-well)
  (let ((new-well (make-instance 'well)))
    (with-slots ((new-pieces pieces)) new-well
      (with-slots ((old-pieces pieces)) old-well
        (let ((w (well-width))
              (h (well-height)))
          (dotimes (y h)
            (dotimes (x w)
              (setf (aref new-pieces (+ x (* y w)))
                    (aref old-pieces (+ x (* y w)))))))))
    new-well))

(defun integrate-tetromino-to-well (tetromino well)
  (let ((new-well (clone-well well)))
    (with-slots ((well-pieces pieces)) new-well
      (with-slots ((tetromino-pieces pieces) color) tetromino
        (dolist (piece tetromino-pieces)
          (with-slots (x y) piece
            (let ((well-pos (piece-well-position tetromino piece)))
              (setf (aref well-pieces (+ (car well-pos) 
                                         (* (cdr well-pos) (well-width))))
                    color))))))
    new-well))

(defun filled-line-p (pieces row-index)
  (notany #'null (subseq pieces
                         (* row-index      (well-width))
                         (* (1+ row-index) (well-width)))))

(defun clear-line-n (well row-index)
  (with-slots (pieces) well
    (if (= row-index 0)
        ;; clear first row
        (loop for x from 0 to (1- (well-width))
           do (setf (aref pieces x) nil))
        ;; clear non-first row and ram all pieces above row-index
        (loop for y from row-index downto 1
           do (loop for x from 0 to (1- (well-width))		 
                 do (setf (aref pieces (+ x (* y      (well-width))))
                          (aref pieces (+ x (* (1- y) (well-width))))))))))

(defun clear-lines (well)
  (let ((new-well (clone-well well))
        (filled-lines (loop for y from 0 to (1- (well-height))
                         when (filled-line-p (slot-value well 'pieces) y) 
                         collect y into lines and count y into lines-num
                         finally (return (cons lines lines-num)))))
    (mapc (curry #'clear-line-n new-well) (car filled-lines))
    (cons (cdr filled-lines) new-well)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; piece stuff

(defclass piece ()
  ((x     :initarg :x     :initform 0)
   (y     :initarg :y     :initform 0))
  (:documentation "Primitive part of tetromino or well contents"))

(defun make-piece (x y)
  (make-instance 'piece :x x :y y))

(defun translate-piece (piece dx dy)
  (with-slots (x y) piece
    (make-piece (+ x dx) (+ y dy))))

(defun translate-pieces (pieces dx dy)
  (mapcar (lambda (p) (translate-piece p dx dy)) pieces))

(defun transform-piece (piece m11 m12 m21 m22)
  (with-slots (x y) piece
    (make-piece (+ (* x m11) (* y m21)) 
                (+ (* x m12) (* y m22)))))

(defun transform-pieces (pieces m11 m12 m21 m22)
  (mapcar (lambda (p) (transform-piece p m11 m12 m21 m22)) pieces))

(defun floor-piece-coordinates (piece)
  (with-slots (x y) piece
    (make-piece (floor x) (floor y))))

(defun rotate-pieces (center pieces m11 m12 m21 m22)
  (let* ((center-x     (first center)) 
         (center-y     (second center))
         (translated   (translate-pieces pieces (- center-x) (- center-y)))
         (rotated      (transform-pieces translated m11 m12 m21 m22))
         (untranslated (translate-pieces rotated center-x center-y)))
    (mapcar #'floor-piece-coordinates untranslated)))

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
   (color           :initarg :color
                    :initform sdl:*green*)
   (falling-counter :initarg :falling-counter :initform 0)))

(defun make-tetromino (x y center pieces color &optional (falling-counter 0))
  (make-instance 'tetromino 
                 :x x 
                 :y y 
                 :center center 
                 :pieces pieces
                 :color color
                 :falling-counter falling-counter))



(defun pieces-coords (type)
  (ecase type
    (I `(,sdl:*cyan*   (1.5 1.5) (0 1) (1 1) (2 1) (3 1)))
    (J `(,*light-blue*   (1.0 1.0) (0 0) (0 1) (1 1) (2 1)))
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
     collect (make-piece (first xy) (second xy))))

(defun make-tetromino-of-type (type x y)
  (let ((coords (pieces-coords type)))
    (make-tetromino x
                    y
                    (center-from-coords coords)
                    (pieces-from-coords coords)
                    (color-from-coords  coords))))

(defun point-inside-of-well-p (x y)
  (let ((well-width  (well-width))
        (well-height (well-height)))
    (and (>= x 0) (< x well-width) (>= y 0) (< y well-height))))

(defun tetromino-overlaps-well-pieces-p (well tetromino)
  "Return true if tetromino clashes with pieces integrated in the well"
  (with-slots ((well-pieces pieces)) well
    (with-slots ((tetromino-pieces pieces)) tetromino
      (dolist (tetromino-piece tetromino-pieces)
        (let ((tetromino-piece-pos (piece-well-position tetromino tetromino-piece)))
          (when (point-inside-of-well-p (car tetromino-piece-pos) 
                                        (cdr tetromino-piece-pos))
            (when (not (null (aref well-pieces (+ (car tetromino-piece-pos) 
                                                  (* (cdr tetromino-piece-pos)
                                                     (well-width))))))
              (return t))))))))

(defun pieces-left-border (tetromino-x pieces)
  (reduce #'min pieces :key (lambda (piece) (+ tetromino-x (slot-value piece 'x)))))

(defun pieces-top-border (tetromino-y pieces)
  (reduce #'min pieces :key (lambda (piece) (+ tetromino-y (slot-value piece 'y)))))

(defun pieces-right-border (tetromino-x pieces)
  (reduce #'max pieces :key (lambda (piece) (+ tetromino-x (slot-value piece 'x)))))

(defun pieces-bottom-border (tetromino-y pieces)
  (reduce #'max pieces :key (lambda (piece) (+ tetromino-y (slot-value piece 'y)))))

(defun pieces-border (tetromino-x tetromino-y pieces)
  (values (pieces-left-border   tetromino-x pieces)
          (pieces-top-border    tetromino-y pieces)
          (pieces-right-border  tetromino-x pieces)
          (pieces-bottom-border tetromino-y pieces)))

(defun pieces-inside-of-well-p (tetromino-x tetromino-y pieces 
                                &key (check-top-border nil))
  "Returns true if tetromino don't overlapps well's borders"
  (multiple-value-bind (l-border t-border r-border b-border)
      (pieces-border tetromino-x tetromino-y pieces)
    (let ((well-width  (car *well-dimensions*))
          (well-height (cdr *well-dimensions*)))
      (and (>= l-border 0) 
           (or (not check-top-border) (>= t-border 0))
           (< r-border well-width) 
           (< b-border well-height)))))

(defun tetromino-overflowed-well-p (tetromino)
  (with-slots (y pieces) tetromino
    (< (pieces-top-border y pieces) 0)))

(defun tetromino-in-allowed-position-p (well tetromino)
  (with-slots (x y pieces) tetromino
    (and (pieces-inside-of-well-p x y pieces)
         (not (tetromino-overlaps-well-pieces-p well tetromino)))))

(defun move-tetromino (well tetromino dx dy &optional reset-falling-counter)
  (let* ((actual-dx (signum dx))	
         (actual-dy (signum dy))
         (moved-tetromino 
          (with-slots (x y center pieces color falling-counter) tetromino
            (make-tetromino (+ x actual-dx) 
                            (+ y actual-dy) 
                            center
                            pieces
                            color
                            (if reset-falling-counter
                                0
                                falling-counter)))))
    (if (tetromino-in-allowed-position-p well moved-tetromino)
        (cons :was-moved moved-tetromino)
        (cons :was-blocked tetromino))))

(defun move-tetromino-left (well tetromino)
  (move-tetromino well tetromino -1 0))

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
         (with-slots (x y center pieces color falling-counter) tetromino
           (make-tetromino x 
                           y
                           center 
                           (if (< direction 0) 
                               (rotate-pieces-ccw center pieces)
                               (rotate-pieces-cw  center pieces))
                           color
                           falling-counter))))
    (if (tetromino-in-allowed-position-p well rotated-tetromino)
        (cons :was-moved   rotated-tetromino)
        (cons :was-blocked tetromino))))

(defun rotate-tetromino-ccw (well tetromino)
  (rotate-tetromino well tetromino -1))

(defun rotate-tetromino-cw (well tetromino)
  (rotate-tetromino well tetromino 1))

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

(defclass blocks-game ()
  ((state             :initform (make-instance 'intro-state))
   (well              :initform (make-instance 'well))
   (curr-tetromino    :initform nil)
   (tetrominos-types-queue :initform (make-tetrominos-types-queue))
   (single-clears-num :initform 0)
   (double-clears-num :initform 0)
   (triple-clears-num :initform 0)
   (quadruples-num      :initform 0)))

(defun replace-curr-tetromino (game tetromino)
  (setf (slot-value game 'curr-tetromino) tetromino))

(defmethod initialize-instance :after ((game blocks-game) &key)
  (let ((spawining-result (spawn-next-tetromino (make-tetrominos-types-queue))))
    (replace-curr-tetromino game (car spawining-result))
    (setf (slot-value game 'tetrominos-types-queue) (cdr spawining-result))))

(defun add-clear (game lines-cleared)
  (incf (slot-value game (ecase lines-cleared
                           (1 'single-clears-num)
                           (2 'double-clears-num)
                           (3 'triple-clears-num)
                           (4 'quadruples-num)))))

(defmethod handle-game-input ((game blocks-game) key)
  (handle-input (slot-value game 'state) key))

(defmethod update-whole-game ((game blocks-game) frame-index)
  (update-game (slot-value game 'state) frame-index))

(defmethod draw-whole-game ((game blocks-game) frame-index)
  (draw-game (slot-value game 'state) frame-index))

(defclass game-state () ())

(defgeneric handle-input (state key))
(defmethod handle-input (state key))

(defgeneric update-game (state frame-index))
(defmethod update-game (state frame-index))
(defmethod update-game :before (state frame-index)
  (handle-slime-requests))

(defgeneric draw-game (state frame-index))
(defmethod draw-game (state frame-index))

(defclass intro-state (game-state) ())

(defmethod handle-input ((state intro-state) key)
  (case key
    (:sdl-key-space (setf (slot-value *game* 'state) (make-instance 'playing-state)))
    (:sdl-key-q (sdl:push-quit-event))))

(defun executable-file-path ()
  #+linux (osicat:read-link #p"/proc/self/exe")
  #-linux (error (concat "executable-file-path is "
                         "not supported for this platform yet")))
(defparameter *sources-dir-path*
  (make-pathname :name nil :type nil
                 :defaults #.(or *compile-file-truename*
                                 *load-truename*)))

(defparameter *assets-dir-type* :development-dir
  "Specifies where to search for assets, can take two values:
   :development-dir - is directory where .lisp sources files are,
   :executable-dir - is directory where dumped image is.")
(defun assets-dir-path ()
  (ecase *assets-dir-type*
    (:development-dir *sources-dir-path*)
    (:executable-dir (make-pathname :name nil :type nil
                                    :defaults (executable-file-path)))))
(defun string-width (str font)
  (* (length str) (sdl:char-width font)))

(defun calc-string-left-border (str font)
  (/ (- (car (calc-window-size)) (string-width str font)) 2))

(defun draw-big-title ()
  (let* ((title-text "BLOCKS")
         (title-colors `(,sdl:*red*   ,*orange*   ,sdl:*yellow* 
                                      ,sdl:*green* ,sdl:*blue* ,*purple*))
         (title-left (calc-string-left-border title-text *biggest-font*)))
    (loop 
       for char across title-text
       for color in title-colors
       for index from 0 
       do (sdl:draw-string-solid-* (string char)
                                   (+ (* index (sdl:char-width *biggest-font*)) title-left)
                                   30
                                   :font *biggest-font*
                                   :color color))))

(defun draw-press-space-to-start ()
  (let* ((text "PRESS SPACE")
         (left (calc-string-left-border text *medium-font*)))
    (sdl:draw-string-solid-* text
                             left
                             (/ (cdr (calc-window-size)) 2)
                             :font *medium-font*
                             :color *orange*)))

(defun draw-author ()
  (let* ((text "BY CYBEVNM")
         (left (calc-string-left-border text *medium-font*)))
    (sdl:draw-string-solid-* text 
                             left
                             (- (cdr (calc-window-size)) 40)
                             :font *medium-font*
                             :color *purple*)))

(defun square-func (x period)
  "Returns -1, 0 or 1 depending on x"
  (signum (- (mod (- x (/ period 2)) period) (/ period 2))))

(defmethod draw-game ((state intro-state) frame-index)
  (draw-big-title)
  (when (> (square-func frame-index 30) 0) 
    (draw-press-space-to-start))
  (draw-author))

(defclass playing-state (game-state) 
  ((paused :initform nil)))

(defmethod handle-input ((state playing-state) key)
  (with-slots (curr-tetromino well) *game*
    (if (slot-value state 'paused)
        (handle-common-keys key state)
        (multiple-value-bind (moving-type moving-remarks moved-tetromino)
            (handle-tetromino-keys key curr-tetromino well)
          (if (not (null moved-tetromino))
              (handle-moved-tetromino moving-type moving-remarks moved-tetromino well)
              (handle-common-keys key state))))))

(defun calc-score (singles doubles triples quadruples)
  (+ (* singles 10) (* doubles 20) (* triples 30) (* quadruples 80)))

(defun calc-game-score ()
  (with-slots (single-clears-num double-clears-num triple-clears-num quadruples-num)
      *game*
    (calc-score single-clears-num double-clears-num triple-clears-num quadruples-num)))

(defun get-level-for-score (score)
  (or (position score
                *levels-scores* 
                :test (lambda (game-score level-score) (> level-score game-score)))
      (1- (length *levels-scores*))))

(defun get-game-level ()
  (get-level-for-score (calc-game-score)))

(defmethod update-game ((state playing-state) frame-index)
  (unless (slot-value state 'paused)
    (with-slots (curr-tetromino well) *game*
      (let ((moving-result (fall-tetromino curr-tetromino well (get-game-level))))
        (handle-moved-tetromino :vertical 
                                (car moving-result) 
                                (cdr moving-result) 
                                well)))))

(defun draw-next-tetromino (next-tetromino)
  (sdl:draw-string-solid-* "NEXT" 
                           *margin* 
                           *margin* 
                           :font *medium-font*
                           :color sdl:*red*)
  (draw-filled-tetromino (slot-value next-tetromino 'pieces) 
                         (slot-value next-tetromino 'color)
                         *margin*
                         (+ *margin* (sdl:char-width *medium-font*) *margin*)))

(defun peek-next-tetromino ()
  (make-tetromino-of-type (car (slot-value *game* 'tetrominos-types-queue)) 0 0))

(defun next-tetromino-color ()
  (slot-value (peek-next-tetromino) 'color))

(defun draw-score ()
  (let* ((text-y (+ *margin*
                    (sdl:get-font-height :font *medium-font*)
                    *margin*
                    (* 4 (cdr *piece-dimensions*)))))
    (sdl:draw-string-solid-* "SCORE"
                             *margin*
                             text-y
                             :font *medium-font*
                             :color sdl:*red*)
    (with-slots (single-clears-num double-clears-num triple-clears-num quadruples-num)
        *game*
      (sdl:draw-string-solid-* (write-to-string (calc-game-score))
                               *margin*
                               (+ text-y 
                                  (sdl:get-font-height :font *medium-font*) 
                                  *margin*)
                               :font *medium-font*
                               :color (next-tetromino-color)))))

(defun draw-level ()
  (let* ((text-y (+ *margin*
                    (sdl:get-font-height :font *medium-font*)
                    *margin*
                    (* 4 (cdr *piece-dimensions*))
                    *margin*
                    (sdl:get-font-height :font *medium-font*)
                    *margin*
                    (sdl:get-font-height :font *medium-font*)
                    *margin*)))
    (sdl:draw-string-solid-* "LEVEL"
                             *margin*
                             text-y
                             :font *medium-font*
                             :color sdl:*red*)
    (sdl:draw-string-solid-* (write-to-string (get-game-level))
                             *margin*
                             (+ text-y
                                (sdl:get-font-height :font *medium-font*)
                                *margin*)
                             :font *medium-font*
                             :color (next-tetromino-color))))

(defun draw-paused-if-required (paused frame-index)
  (when paused
    (when (> (square-func frame-index 30) 0) 
      (let* ((text "PAUSED")
             (text-w (string-width text *medium-font*))
             (text-x (+ (car (well-window-pos)) 
                        (/ (- (car (well-pixels-size)) 
                              text-w)
                           2)))
             (text-y (+ (cdr (well-window-pos)) 
                        (/ (- (cdr (well-pixels-size)) 
                              (sdl:get-font-height :font *medium-font*))
                           2))))
        (sdl:draw-string-solid-* text
                                 text-x
                                 text-y
                                 :font *medium-font*
                                 :color sdl:*red*)))))

(defmethod draw-game ((state playing-state) frame-index)
  (with-slots (well curr-tetromino tetrominos-types-queue) *game*
    (draw-well well)
    (with-slots (pieces color x y) curr-tetromino
      (draw-filled-tetromino-at-well-pos pieces color x y))
    (with-slots (pieces color x y) (drop-tetromino well curr-tetromino)
      (draw-projected-tetromino-at-well-pos pieces color x y))
    (draw-next-tetromino (peek-next-tetromino))
    (draw-score)
    (draw-level)
    (draw-paused-if-required (slot-value state 'paused) frame-index)))

(defclass game-over-state (game-state) ())

(defmethod handle-input ((state game-over-state) key)
  (case key
    (:sdl-key-q (sdl:push-quit-event))))

(defun draw-game-over ()
  (let* ((first-line "GAME")
         (second-line "OVER")
         (text-x (/ (- (car (calc-window-size)) (string-width first-line *biggest-font*))
                    2)))
    (sdl:draw-string-solid-* first-line
                             text-x
                             (/ (cdr (well-pixels-size)) 3)
                             :font *biggest-font*
                             :color sdl:*red*)
    (sdl:draw-string-solid-* second-line
                             text-x
                             (- (cdr (well-pixels-size))
                                (/ (cdr (well-pixels-size)) 3))
                             :font *biggest-font*
                             :color sdl:*red*)))

(defmethod draw-game ((state game-over-state) frame-index)
  (draw-game-over))

(defun replace-well (game well)
  (setf (slot-value game 'well) well))

(defun handle-slime-requests ()
  #+swank
  (let ((connection (or swank::*emacs-connection* 
                        (swank::default-connection))))
    ;;(when (and connection (not (eql swank:*communication-style* :spawn)))
    ;;  (swank::handle-requests connection t))))
    (when connection
      (swank::handle-requests connection t))))

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

(defun handle-common-keys (key state)
  (with-slots (well curr-tetromino) *game*
    (case key
      (:sdl-key-q (sdl:push-quit-event))
      (:sdl-key-d (dump-well-to-stdout well))
      (:sdl-key-c (clear-well))
      (:sdl-key-p (with-slots (paused) state
                    (print "pause triggered")
                    (setf paused (not paused)))))))

(defun get-falling-threshold-for-level (level)
  (let ((thresholds-list-len (length *levels-falling-thresholds*)))
    (nth (if (> level thresholds-list-len) (1- thresholds-list-len) level)
         *levels-falling-thresholds*)))

(defun first-or-null (lst)
  (and lst (first lst)))

(defun fall-tetromino (tetromino well level)
  (with-slots (x y center pieces color falling-counter) tetromino
    (let ((new-falling-counter (1+ falling-counter)))
      (if (>= new-falling-counter (get-falling-threshold-for-level level))
          (move-tetromino-down well tetromino)
          (cons :nothing-happened 
                (make-tetromino x y center pieces color (1+ falling-counter)))))))

(defun update-game-objects (well tetromino tetrominos-types-queue)
  (let ((clearing-result (clear-lines well)))
    (replace-well *game* (cdr clearing-result))
    (replace-curr-tetromino *game* tetromino)
    (when (> (car clearing-result) 0)
      (add-clear *game* (car clearing-result))))
  (when tetrominos-types-queue
    (setf (slot-value *game* 'tetrominos-types-queue) tetrominos-types-queue)))

(defun make-tetrominos-types-queue ()
  (random-list-permutation '(I J L O S T Z)))

(defun spawn-next-tetromino (tetrominos-types-queue)
  (let ((actual-tetrominos-types-queue
         (if (null (cdr tetrominos-types-queue))
             (append tetrominos-types-queue (make-tetrominos-types-queue))
             tetrominos-types-queue)))
    (cons (make-tetromino-of-type (car actual-tetrominos-types-queue) 5 -2)
          (cdr actual-tetrominos-types-queue))))

(defun handle-moved-tetromino (moving-type moving-remarks moved-tetromino well)
  (ecase moving-remarks
    (:was-moved   (update-game-objects well moved-tetromino nil))
    (:was-blocked (when (eql moving-type :vertical) 
                    (if (tetromino-overflowed-well-p moved-tetromino)
                        (setf (slot-value *game* 'state) (make-instance 'game-over-state))
                        (let ((spawning-result
                               (spawn-next-tetromino (slot-value *game* 'tetrominos-types-queue))))
                          (update-game-objects (integrate-tetromino-to-well moved-tetromino well)
                                               (car spawning-result)
                                               (cdr spawning-result))))))
    (:nothing-happened (update-game-objects well moved-tetromino nil))))

(defun draw-piece (drawing-func color window-x window-y)
  (funcall drawing-func 
           window-x 
           window-y
           (car *piece-dimensions*) 
           (cdr *piece-dimensions*)
           :color color))

(defun draw-tetromino (piece-drawing-func pieces color window-x window-y)  
  (dolist (piece pieces) 
    (with-slots (x y) piece      
      (draw-piece piece-drawing-func 
                  color 
                  (+ (* x (car *piece-dimensions*)) window-x)
                  (+ (* y (cdr *piece-dimensions*)) window-y)))))

(defun fixed-draw-box-* (x y w h &key color)
  "sdl:draw-box-* has bug and this function fixes it"
  (sdl:draw-box-* x y (1- w) (1- h) :color color))

(defun draw-filled-tetromino (pieces color window-x window-y)
  (draw-tetromino #'fixed-draw-box-*
                  pieces
                  color
                  window-x
                  window-y))

(defun piece-inside-of-well (well-x well-y piece)
  (with-slots (x y) piece
    (and (>= (+ well-x x) 0)
         (>= (+ well-y y) 0)
         (<  (+ well-x x) (car *well-dimensions*))
         (<  (+ well-y y) (cdr *well-dimensions*)))))

(defun well-to-pixels-coordinates (x y)
  (cons (+ (car (well-window-pos)) (* x (car *piece-dimensions*)))
        (+ (cdr (well-window-pos)) (* y (cdr *piece-dimensions*)))))

(defun draw-tetromino-at-well-pos (tetromino-drawing-func pieces color well-x well-y)
  (let ((passed-pieces 
         (remove-if-not (curry #'piece-inside-of-well well-x well-y) pieces)))
    (let ((window-xy (well-to-pixels-coordinates well-x well-y)))
      (funcall tetromino-drawing-func
               passed-pieces
               color
               (car window-xy) 
               (cdr window-xy)))))

(defun draw-filled-tetromino-at-well-pos (pieces color well-x well-y)
  (draw-tetromino-at-well-pos #'draw-filled-tetromino pieces color well-x well-y))

(defun draw-projected-tetromino (pieces color window-x window-y)
  (draw-tetromino #'sdl:draw-rectangle-* pieces color window-x window-y))

(defun draw-projected-tetromino-at-well-pos (pieces color well-x well-y) 
  (draw-tetromino-at-well-pos #'draw-projected-tetromino pieces color well-x well-y))

(defun draw-well-border (well well-window-pos)
  (sdl:draw-rectangle-* (1- (car well-window-pos))
                        (1- (cdr well-window-pos))
                        (+ (car (well-pixels-size)) 2)
                        (+ (cdr (well-pixels-size)) 2)))

(defun draw-well-pieces (well well-window-pos)
  (with-slots (pieces) well
    (let ((w (well-width))
          (h (well-height)))
      (dotimes (y h)
        (dotimes (x w)
          (let ((color (aref pieces (+ x (* y w)))))
            (unless (null color)
              (draw-piece #'fixed-draw-box-* 
                          color
                          (+ (car well-window-pos)
                             (* (car *piece-dimensions*) x))
                          (+ (cdr well-window-pos)
                             (* (cdr *piece-dimensions*) y))))))))))

(defun draw-well-grid (well)
  (let ((v-lines-num (1- (well-width)))
        (h-lines-num (1- (well-height)))
        (well-pos (well-window-pos)))
    ;; vertical lines
    (dotimes (x v-lines-num)
      (sdl:draw-vline (+ (car well-pos) (* (car *piece-dimensions*) (1+ x)))
                      (cdr well-pos)
                      (1- (+ (cdr well-pos) (cdr (well-pixels-size))))
                      :color *well-grid-color*))
    ;; horizontal lines
    (dotimes (y h-lines-num)
      (sdl:draw-hline (car well-pos)
                      (1- (+ (car well-pos) (car (well-pixels-size))))
                      (+ (cdr well-pos) (* (cdr *piece-dimensions*) (1+ y)))
                      :color *well-grid-color*))))

(defun draw-well (well)
  (draw-well-pieces well (well-window-pos))
                                        ;(draw-well-grid   well)
  (draw-well-border well (well-window-pos)))

(defun dump-well-to-stdout (well)
  (with-slots (pieces) well
    (let ((w (well-width))
          (h (well-height)))
      (dotimes (y h)
        (format t "~%")
        (dotimes (x w)
          (if (null (aref pieces (+ x (* y w))))
              (format t "~a" ".")
              (format t "~a" "x")))))
    (format t "~%")))

(defun clear-well ()
  (replace-well *game* (make-instance 'well)))

(defun make-font (file-name char-width char-height)
  (let ((chars-num 48))
    (sdl:initialise-font 
     (make-instance 'sdl:simple-font-definition
                    :width char-width :height char-height
                    :character-map "ABCDEFGHIJKLMNOPQRSTUVWXYZ:'!?_-,.()#~0123456789"
                    :character-mask (loop for y from 0 below 1
                                       append (loop for x from 0 below chars-num
                                                 collect (list (* x char-width) 
                                                               (* y char-height) 
                                                               char-width 
                                                               char-height)))
                    :color-key (sdl:color :r 99 :g 0 :b 0)
                    :filename (sdl:create-path file-name
                                               (assets-dir-path))))))

(defvar *biggest-font* nil)
(defvar *big-font* nil)
(defvar *medium-font* nil)
(defun initialise-fonts ()
  (setf *biggest-font* (make-font "biggest-font.bmp" 32 40)
        *big-font*     (make-font "big-font.bmp" 24 30)
        *medium-font*  (make-font "medium-font.bmp" 16 20)))

;;; Entry point
(defun blocks-main ()
  (sdl:with-init ()
    (let ((*game* (make-instance 'blocks-game))
          (window-size (calc-window-size))
          (frame-index 0))      
      (sdl:window (car window-size) 
                  (cdr window-size)
                  :fps (make-instance 'sdl:fps-fixed :target-frame-rate 30))
      (sdl:enable-key-repeat 100 50)
                                        ;(setf (sdl:frame-rate) 30)
      (sdl:initialise-default-font sdl:*font-5x7*)
      (initialise-fonts)
      (sdl:with-events (:poll)
        (:quit-event () t)
        (:video-expose-event () (sdl:update-display))
        (:key-down-event (:key key) (handle-game-input *game* key))
        (:idle ()
               (incf frame-index)
               (sdl:clear-display *background-color*)
               (update-whole-game *game* frame-index)
               (draw-whole-game *game* frame-index)
               (sdl:update-display))))))
(defun blocks-image-main ()
  "Should be used to start application from the dumped image"
  (let ((*assets-dir-type* :executable-dir))
    (blocks-main)))
