#!/usr/bin/sbcl --script
; Fix a waterlogged transponder.

; load libraries
(load "~/quicklisp/setup.lisp")
(ql:quickload "str")

; global variables
(defvar register (list 1))
; (defvar filename "test-basic.txt")
; (defvar filename "test.txt")
(defvar filename "input.txt")

; helper function to interpret a command
(defun interpret-command (command val)
  (let ((words (str:words command)))
    (cond
      ; noop takes one clock cycle but doesn't change the value
      ((string= (first words) "noop") (list val))
      ; addx takes two clock cycles and changes the value (eventually)
      ((string= (first words) "addx") (list val (+ val (parse-integer (second words)))))
    )
  )
)

; walk through inputs, collecting register values
(with-open-file (stream filename)
  (do
    ((nextline (read-line stream nil)  (read-line stream nil)))
    ((null nextline))
    ; walk through the inputs, collecting register values
    (let ((X (nth (1- (length register)) register)))
      (setq register (append register (interpret-command nextline X)))
    )
  )
)

; for part A we want a proxy for signal strength - index * register value every 20th cycle
(defun calc-signal-strength (vals)
  (let ((res 0))
    (dotimes (i (length vals))
      (let ((cycle (1+ i)))
        (if
          ; check if this is one of the cycles we care about
          (eq (mod (+ cycle 20) 40) 0)
          (progn
            (format t "Cycle ~A value ~A~%" cycle (nth i vals))
            (incf res (* cycle (nth i vals)))
          )
        )
      )
    )
    ; return signal strength sum
    res
  )
)

; Part B requires us to check when the clock cycle and register are aligned
(defun track-sprite (register)
  (let ((str ""))
    ; iterate through register, marking pixels as we go
    (dotimes (i (length register))
      ; add a '#' if our sprite is in a good position, else '.'
      (let ((val (nth i register))
            (cycle (mod i 40)))
        (if
          (< (abs (- val cycle)) 2)
          (setf str (concatenate 'string str (list #\#)))
          (setf str (concatenate 'string str (list #\.)))
        )
      )
      ; add a space to split later
      (if
        (eq (mod (1+ i) 40) 0)
        (setf str (concatenate 'string str (list #\Space)))
      )
    )
    (format t "~A~%" str)
    (str:words str)
  )
)

(format t "Register Values: ~A~%" register)
(format t "Signal Strength Sum (Part A): ~A~%" (calc-signal-strength register))
(format t "Track Sprite (Part B): ~A~%" (track-sprite register))



