#!/usr/bin/sbcl --script
; Simulate rope trajectories as a coping mechanism.

; load libraries
(load "~/quicklisp/setup.lisp")
(ql:quickload "str")

; main input file
; (defvar filename "test.txt")
; (defvar filename "test2.txt")
(defvar filename "input.txt")

(defstruct (point
  (:print-function (lambda (p s k) (format s "(~A,~D)" (point-x p) (point-y p)))))
  x ; X location / distance
  y ; Y location / distance
)
(defun point-add (a b)
  (make-point :x (+ (point-x a) (point-x b)) :y (+ (point-y a) (point-y b)))
)
(defun point-subtract (a b)
  (make-point :x (- (point-x a) (point-x b)) :y (- (point-y a) (point-y b)))
)
(defun point-equal (a b)
  (and (eq (point-x a) (point-x b)) (eq (point-y a) (point-y b)))
)

; return the appropriate action for Tail given Head location
(defun lookup-action (p)
  (let ((i (point-x p)) (j (point-y p)))
    (cond
      ; no action required if i and j are less than 2
      ((and (< (abs i) 2) (< (abs j) 2)) (make-point :x 0 :y 0))
      ; if both i and j are equal to 2, move diagonally
      ((and (eq (abs i) 2) (eq (abs j) 2)) (make-point :x (/ i 2) :y (/ j 2)))
      ; if i == 2 and j < 2
      ((eq (abs i) 2) (make-point :x (/ i 2) :y j))
      ; if j == 2 and i < 2
      ((eq (abs j) 2) (make-point :x i :y (/ j 2)))
    )
  )
)

; move HEAD by action
(defun move-head (head dir)
  (cond
    ((string= dir "U") (point-add head (make-point :x 0 :y 1)))
    ((string= dir "D") (point-add head (make-point :x 0 :y -1)))
    ((string= dir "R") (point-add head (make-point :x 1 :y 0)))
    ((string= dir "L") (point-add head (make-point :x -1 :y 0)))
  )
)

; calculate the number of unique points within a list
(defun count-unique (points)
  (format t "POINTS: ~A~%" points)
  (length (remove-duplicates points :test #'point-equal))
)

; iterate through input and collect actions
(with-open-file (stream filename)
  (do
    ( ; local variable nextline is initialized with the first line
      (nextline (read-line stream nil)  (read-line stream nil))
      (points (list
                (list (make-point :x 0 :y 0))
                (list (make-point :x 0 :y 0))
                (list (make-point :x 0 :y 0))
                (list (make-point :x 0 :y 0))
                (list (make-point :x 0 :y 0))
                (list (make-point :x 0 :y 0))
                (list (make-point :x 0 :y 0))
                (list (make-point :x 0 :y 0))
                (list (make-point :x 0 :y 0))
                (list (make-point :x 0 :y 0))))     ; all positions TAIL has been (World coords)
      (knots (list
               (make-point :x 0 :y 0)
               (make-point :x 0 :y 0)
               (make-point :x 0 :y 0)
               (make-point :x 0 :y 0)
               (make-point :x 0 :y 0)
               (make-point :x 0 :y 0)
               (make-point :x 0 :y 0)
               (make-point :x 0 :y 0)
               (make-point :x 0 :y 0)
               (make-point :x 0 :y 0)))              ; location of TAIL in World coordinates
    )
    ( ; stopping condition and actions
      (null nextline) ; quits when nextline is null, ie eof
      ; print out final location (for debugging)
      (format t "Final Locations: (~A)~%" knots)
      (format t "Unique 1st TAIL locations (Part A): ~A~%" (count-unique (nth 1 points)))
      (format t "Unique 9th TAIL locations (Part B): ~A~%" (count-unique (nth 9 points)))
    )
    ; body of loop; calculate next Head Location and determine Tail action
    (let ((words (str:words nextline)))
      (let ((dir (first words)) (N (parse-integer (second words))))
        ; iterate N times
        (dotimes (n N)
          ; iterate through our rope sections
          (dotimes (k (length knots))
            (if
              ; if k==0 this is the true HEAD - just move it
              (eq k 0)
              (setf (elt knots k) (move-head (nth k knots) dir))
              ; otherwise determine the relative location of our head
              (let ((rel-head (point-subtract (nth (1- k) knots) (nth k knots))))
                ; determine action from relative location of head
                (setf (elt knots k) (point-add (nth k knots) (lookup-action rel-head)))
              )
            )
            ; save whatever update we made to the points list
            (setf (elt points k) (append (nth k points) (list (nth k knots))))
          )
        )
      )
    )
  )
)

