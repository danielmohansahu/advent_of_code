#!/usr/bin/sbcl --script
; Day #5: Help the elves rearrange their cargo hold.

; hardcoded puzzle input to bypass parsing pain
;
; Test Input:
;
;     [D]    
; [N] [C]    
; [Z] [M] [P]
;  1   2   3 
;
; (defvar filename "test.txt")
; (defvar states_a (list (list) (list "N" "Z") (list "D" "C" "M") (list "P")))
; (defvar states_b (copy-list states_a))

; My Input:
;
;                 [V]     [C]     [M]
; [V]     [J]     [N]     [H]     [V]
; [R] [F] [N]     [W]     [Z]     [N]
; [H] [R] [D]     [Q] [M] [L]     [B]
; [B] [C] [H] [V] [R] [C] [G]     [R]
; [G] [G] [F] [S] [D] [H] [B] [R] [S]
; [D] [N] [S] [D] [H] [G] [J] [J] [G]
; [W] [J] [L] [J] [S] [P] [F] [S] [L]
;  1   2   3   4   5   6   7   8   9
;
(defvar filename "input.txt")
(defvar states_a (list
  (list)
  (list "V" "R" "H" "B" "G" "D" "W")
  (list "F" "R" "C" "G" "N" "J")
  (list "J" "N" "D" "H" "F" "S" "L")
  (list "V" "S" "D" "J")
  (list "V" "N" "W" "Q" "R" "D" "H" "S")
  (list "M" "C" "H" "G" "P")
  (list "C" "H" "Z" "L" "G" "B" "J" "F")
  (list "R" "J" "S")
  (list "M" "V" "N" "B" "R" "S" "G" "L")
))
(defvar states_b (copy-list states_a))

; execute the given operations for Part A
;  -> "MOVE num FROM from TO to"
(defun execute_a (states num from to)
  (dotimes (n num)
    (push (pop (nth from states)) (nth to states))
  )
)

; execute the given operations for Part B
;  in this case we want to preserve order for multi-box moves
(defun execute_b (states num from to)
  (let ((tmp (list)))
    ; pop to an intermediate list to keep the ordering
    (dotimes (n num)
      (push (pop (nth from states)) tmp)
    )
    ; reverse push to target list
    (dotimes (n num)
      (push (pop tmp) (nth to states))
    )
  )
)

; convenience function to convert a space separated string to an array of vals
(defun parse-string-to-ints (line)
  (loop
    :with n := (length line)
    :for pos := 0 :then chars
    :while (< pos n)
    :for (integer chars) := (multiple-value-list
            (read-from-string line nil nil :start pos))
    :collect integer)
)

; print out the top cargo for each state
(defun print_top (lists)
  (loop for state in lists do (format t "~A " (first state)))
  (format t "~%")
)

; iterate through input, operating on states as we go
(with-open-file (stream filename)
  (do
    ( ; local variable nextline is initialized with the first line
      (nextline (read-line stream nil)  (read-line stream nil))
    )
    ( ; stopping condition and actions
      (null nextline) ; quits when nextline is null, ie eof
      (format t "Top Cargo (Part A): ")
      (print_top states_a)
      (format t "Top Cargo (Part B): ")
      (print_top states_b)
    )
    ; body of loop; clean string and accumulate if a is within b
    (let ((cleaned (remove-if #'both-case-p nextline)))
      (apply (lambda (a b c) (execute_a states_a a b c)) (parse-string-to-ints cleaned))
      (apply (lambda (a b c) (execute_b states_b a b c)) (parse-string-to-ints cleaned))
    )
  )
)


