#!/usr/bin/sbcl --script
; Day Three - Fixing a botched packing job.

; input(s)
; (defvar filename "test.txt")
(defvar filename "input.txt")

; a helper function to convert a char to our priority encoding
(defun encode_char (c)
  ; convert ascii encoding to our local encoding; no handling for non-alpha values
  (let ((code (char-int c)))
    (if (> code 96)
      ; lowercase - return offset
      (- code 96)
      ; uppercase - return with different offset
      (- code 38)
    )
  )
)

; a helper function to convert a value back into a char
(defun decode_int (int)
  (if (> int 26)
    ; uppercase
    (code-char (+ int 38))
    ; lowercase
    (code-char (+ int 96))
  )
)

; a helper function to convert a character string to a binary number
(defun string_to_binary (str)
  (let ((num 0))
    (loop for c across (remove-duplicates str) do (incf num (expt 2 (encode_char c))))
    num
  )
)

; iterate through input, accumulating the total priority of misplaced objects
(with-open-file (stream filename)
  (do
    ( ; local variable nextline is initialized with the first line
      (nextline (read-line stream nil)  (read-line stream nil))
      (priority 0)
    )
    ( ; stopping condition and actions
      (null nextline) ; quits when nextline is null, ie eof
      (format t "Total priority (Part A): ~A~%" priority)
    )
    ; body of loop; accumulate scores
    (let ((middle (/ (length nextline) 2)))
      ; split into left and right strings:
      (let ((left (string_to_binary (subseq nextline 0 middle))) (right (string_to_binary (subseq nextline middle))))
        ; get the common character (priority) - log base 2 of the bitwise AND
        ; debugging
        ; (format t "Priority of common char: ~A~%" (log (logand left right) 2))
        (incf priority (log (logand left right) 2))
      )
    )
  )
)

; Part 2, iterate again, this time finding the common item in all groups
(with-open-file (stream filename)
  (do
    ( ; local variable nextline is initialized with the first line
      (nextline (read-line stream nil)  (read-line stream nil))
      (idx 0)
      (priority 0)
      (running_and 0)
    )
    ( ; stopping condition and actions
      (null nextline) ; quits when nextline is null, ie eof
      ; add final value
      (incf priority (log running_and 2))
      (format t "Total priority (Part B): ~A~%" priority)
    )

    ; first time initialization
    (if (eq running_and 0) (setq running_and (string_to_binary nextline)))

    ; check if we reached the next group and need to reset 
    (if (eq idx 3)
      (progn
        (format t "Resetting - adding priority ~A~%" (log running_and 2))
        (incf priority (log running_and 2))
        (setq idx 0)
        (setq running_and (string_to_binary nextline))
      )
    )

    ; body of loop; perform bitwise and
    (setq running_and (logand running_and (string_to_binary nextline)))
    ; increment
    (incf idx)
  )
)

