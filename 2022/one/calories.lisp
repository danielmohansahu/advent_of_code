#!/usr/bin/sbcl --script
; Day #1: Get calories carried by elf expedition.

; set input
; (defvar filename "test.txt")
(defvar filename "input.txt")

; initialize list of calories
(defvar calories '())

; iterate through the file, adding grouped calories
(with-open-file (stream filename)
  (do
    ( ; local variable nextline is initialized with the first
      ;    line of the file, and after each pass through the
      ;    loop is gets the next line of the file
      (nextline (read-line stream nil)  (read-line stream nil))
      (sum 0)
    )

    ( ; stopping condition and actions
      (null nextline) ; quits when nextline is null, ie eof
      (setq calories (append calories (list sum))) ; save the final sum
      (format t "done!~%") ; message to print after quitting
    )

    ; body of loop; accumulate or reset sums if this is an empty line
    (if
      (string= "" nextline)
      ; if this is a newline append our current sum to the list and reset
      (progn
        (setq calories (append calories (list sum)))
        (setq sum 0)
      )
      ; otherwise just update sume
      (setq sum (+ sum (parse-integer nextline)))
    )
    ; Debugging
    ; (format t "current line: ~A~%" nextline)
    ; (format t "sum         : ~A~%" sum)
  )
)

; print out the largest value and index
(defun printmax (vals)
  (let
    ((max_idx 0) (max_val 0) (idx 0))
    (loop
      ; count which iteration we're on
      for elem in vals
      ; body of the loop: prints the square of the current value
      do (progn
           (if
             (> elem max_val)
             (progn
               (setq max_val elem)
               (setq max_idx idx)
               (format t "max (idx,val): (~A,~A)~%" max_idx max_val)
             )
           )
           (setq idx (+ idx 1))
         )
    )
    ; (format t "(idx,val): (~A,~A)" max_idx max_val)
  )
)
(printmax calories)

; part #2 wants the top three elves. since we don't care about indices, just sort:
(let
  ((sorted (sort calories '>)))
  (format t "Top 3 elves have ~A calories~%" (+ (first sorted) (second sorted) (third sorted)))
)





















