#!/usr/bin/sbcl --script
; Simulate rope trajectories as a coping mechanism.

; load libraries
(load "~/quicklisp/setup.lisp")
(ql:quickload "str")

; main input file
; (defvar filename "test.txt")
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
      (points (list (make-point :x 0 :y 0)))  ; all positions TAIL has been (World coords)
      (tail (make-point :x 0 :y 0))           ; location of TAIL in World coordinates
      (head (make-point :x 0 :y 0))           ; location of HEAD relative to TAIL
    )
    ( ; stopping condition and actions
      (null nextline) ; quits when nextline is null, ie eof
      ; print out final location (for debugging)
      (format t "Final Locations (HEAD,TAIL): (~A,~A)~%" tail (point-add head tail))
      (format t "Unique TAIL locations (Part A): ~A~%" (count-unique points))
    )
    ; body of loop; calculate next Head Location and determine Tail action
    (let ((words (str:words nextline)))
      (let ((dir (first words)) (N (parse-integer (second words))))
        ; iterate N times
        (dotimes (n N)
          (format t "HEAD: ~A, processing ~A:~%" head dir)
          ; calculate new head location
          (let ((new-head (move-head head dir)))
            (format t "  head is now at ~A~%" new-head)
            ; get TAIL action
            (let ((tail-action (lookup-action new-head)))
              (format t "  tail action: ~A~%" tail-action)
              ; update relative position of HEAD and global position of TAIL
              (setq tail (point-add tail tail-action))
              (setq head (point-subtract new-head tail-action))
              (format t "  head is now: ~A~%" head)
              (format t "  tail is now: ~A~%" tail)
              ; save point
              (setq points (append points (list tail)));
            )
          )
        )
      )
    )
  )
)

