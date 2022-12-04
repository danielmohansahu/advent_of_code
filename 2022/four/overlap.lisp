#!/usr/bin/sbcl --script
; Day #4 - Finding overlapping cleaning schedules.

; define input files
; (defvar filename "test.txt")
(defvar filename "input.txt")

; https://stackoverflow.com/questions/1495475/parsing-numbers-from-strings-in-lisp
(defun parse-string-to-floats (line)
  (loop
    :with n := (length line)
    :for pos := 0 :then chars
    :while (< pos n)
    :for (float chars) := (multiple-value-list
            (read-from-string line nil nil :start pos))
    :collect float)
)

; For Part A, we want to determine if one sequence contains the other.
(defun contains (a_start a_end b_start b_end)
  (let ((result 0))
    ; check if a contains b
    (if (and (<= a_start b_start) (>= a_end b_end))
      (setq result 1)
    )
    ; check if b contains a
    (if (and (<= b_start a_start) (>= b_end a_end))
      (setq result 1)
    )
    ; return result
    result
  )
)

; For Part B, we want to determine if one sequence contains the other.
(defun overlaps (a_start a_end b_start b_end)
  (let ((result 1))
    ; check if there's no possibility of overlap
    (if (or (< a_end b_start) (> a_start b_end))
      (setq result 0)
    )
    ; return result
    result
  )
)

; iterate through input, accumulating the total priority of misplaced objects
(with-open-file (stream filename)
  (do
    ( ; local variable nextline is initialized with the first line
      (nextline (read-line stream nil)  (read-line stream nil))
      (subsumations 0)
      (overlappings 0)
    )
    ( ; stopping condition and actions
      (null nextline) ; quits when nextline is null, ie eof
      (format t "Total subsumed (Part A): ~A~%" subsumations)
      (format t "Total overlaps (Part B): ~A~%" overlappings)
    )
    ; body of loop; clean string and accumulate if a is within b
    (let ((cleaned (substitute #\Space #\, (substitute #\Space #\- nextline))))
      (incf subsumations (apply #'contains (parse-string-to-floats cleaned)))
      (incf overlappings (apply #'overlaps (parse-string-to-floats cleaned)))
    )
  )
)

; Part 2, iterate again, this time finding the common item in all groups
