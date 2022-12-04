#!/usr/bin/sbcl --script
; Day #2 - Cheating at Rock Paper Scissors???

; input file
(defvar filename "input.txt")

(defun calc_score_a (action)
  (let
    ((score 0))
    ; A,B,C -> X,Y,Z -> Rock, Paper, Scissors
    (cond ((string= action "A X") (setq score (+ 1 3)))
          ((string= action "A Y") (setq score (+ 2 6)))
          ((string= action "A Z") (setq score (+ 3 0)))
          ((string= action "B X") (setq score (+ 1 0)))
          ((string= action "B Y") (setq score (+ 2 3)))
          ((string= action "B Z") (setq score (+ 3 6)))
          ((string= action "C X") (setq score (+ 1 6)))
          ((string= action "C Y") (setq score (+ 2 0)))
          ((string= action "C Z") (setq score (+ 3 3)))
    )
    ; (format t "If the action is ~A my score is ~A~%" action score)
    score
  )
)

(defun calc_score_b (action)
  (let
    ((score 0))
    ; A,B,C -> Rock, Paper, Scissors
    ; X,Y,Z -> Lose, Draw, Win
    (cond ((string= action "A X") (setq score (+ 3 0)))
          ((string= action "A Y") (setq score (+ 1 3)))
          ((string= action "A Z") (setq score (+ 2 6)))
          ((string= action "B X") (setq score (+ 1 0)))
          ((string= action "B Y") (setq score (+ 2 3)))
          ((string= action "B Z") (setq score (+ 3 6)))
          ((string= action "C X") (setq score (+ 2 0)))
          ((string= action "C Y") (setq score (+ 3 3)))
          ((string= action "C Z") (setq score (+ 1 6)))
    )
    ; (format t "If the action is ~A my score is ~A~%" action score)
    score
  )
)

; from the given test in problem statement - should be 15
(format t "Test Total (Part A): ~A~%" (+ (calc_score_a "A Y") (calc_score_a "B X") (calc_score_a "C Z")))
(format t "Test Total (Part A): ~A~%" (+ (calc_score_b "A Y") (calc_score_b "B X") (calc_score_b "C Z")))

; iterate through input, accumulating scores:
(with-open-file (stream filename)
  (do
    ( ; local variable nextline is initialized with the first line
      (nextline (read-line stream nil)  (read-line stream nil))
      (sum_a 0)
      (sum_b 0)
    )
    ( ; stopping condition and actions
      (null nextline) ; quits when nextline is null, ie eof
      (format t "Total score for ~A (Part A): ~A~%" filename sum_a)
      (format t "Total score for ~A (Part B): ~A~%" filename sum_b)
    )
    ; body of loop; accumulate scores
    (setq sum_a (+ sum_a (calc_score_a nextline)))
    (setq sum_b (+ sum_b (calc_score_b nextline)))
  )
)

