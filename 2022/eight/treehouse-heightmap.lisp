#!/usr/bin/sbcl --script
; Building a treehouse to ... save Christmas?

; load libraries
(load "~/quicklisp/setup.lisp")

; main input file
; (defvar filename "test.txt")
(defvar filename "input.txt")

; helper function to convert a string to a row of heights
(defun string-to-heights (str)
  (map 'list #'digit-char-p str)
)

; helper function to check if a tree is visible
(defun is-visible (x y grid)
  ; access value by index
  (defun get-cell (x y)
    (nth y (nth x grid))
  )
  (let ((h (get-cell x y))
        (x-size (length grid))
        (y-size (length (first grid))))
    (cond
      ; edge cases, literally, are visible
      ((eq x 0) 1)
      ((eq y 0) 1)
      ((eq x (- x-size 1)) 1)
      ((eq y (- y-size 1)) 1)
      ; otherwise, we need to scan cardinal directions
      ; check if everyone above is smaller
      ((< (apply #'max (loop for n from 0 below x collect (get-cell n y))) h) 1)
      ; check if everyone below is smaller
      ((< (apply #'max (loop for n from (+ x 1) below x-size collect (get-cell n y))) h) 1)
      ; check if everyone to the left is smaller
      ((< (apply #'max (loop for n from 0 below y collect (get-cell x n))) h) 1)
      ; check if everyone to the right is smaller
      ((< (apply #'max (loop for n from (+ y 1) below y-size collect (get-cell x n))) h) 1)
      ; no dice - we're hidden!
      (t 0)
    )
  )
)

; helper function to count all visible elements in a grid
(defun count-visible (grid)
  ; initialize counter of visible cells
  (let ((cnt 0))
    ; loop over rows
    (dotimes (x (length grid))
      (dotimes (y (length (first grid)))
        (format t "(~A,~A) visible: ~A~%" x y (is-visible x y grid))
        (incf cnt (is-visible x y grid))
      )
    )
    ; return visible count
    cnt
  )
)

; iterate through input and convert to a grid
(with-open-file (stream filename)
  (do
    ( ; local variable nextline is initialized with the first line
      (nextline (read-line stream nil)  (read-line stream nil))
      (grid NIL)
    )
    ( ; stopping condition and actions
      (null nextline) ; quits when nextline is null, ie eof
      ; Part A - count visible elements
      (format t "Visible Trees: ~A~%" (count-visible grid))
    )
    ; body of loop; add successive rows to the grid
    (setq grid (append grid (list (string-to-heights nextline))))
  )
)

