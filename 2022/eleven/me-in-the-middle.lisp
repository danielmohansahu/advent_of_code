#!/usr/bin/sbcl --script
; Simulating getting punked by some Monkeys.

; core monkey class
(defstruct monkey
  items       ; list of items (in order) with their current priority
  operation   ; how this monkey's handling affects item worry level
  test        ; test operation
  inspections ; number of inspections to date
)

; how we update post-handled item worry level
(defun boredom-factor (worry)
  (floor worry 3)
)

; initialize our monkeys (hardcoded)
; TEST MONKEYS:
; (defvar all-monkeys
;   (list
;     (make-monkey :items (list 79 98)
;                  :inspections 0
;                  :operation (lambda (old) (* old 19))
;                  :test (lambda (val) (if (eq 0 (mod val 23)) 2 3)))
;     (make-monkey :items (list 54 65 75 74)
;                  :inspections 0
;                  :operation (lambda (old) (+ old 6))
;                  :test (lambda (val) (if (eq 0 (mod val 19)) 2 0)))
;     (make-monkey :items (list 79 60 97)
;                  :inspections 0
;                  :operation (lambda (old) (* old old))
;                  :test (lambda (val) (if (eq 0 (mod val 13)) 1 3)))
;     (make-monkey :items (list 74)
;                  :inspections 0
;                  :operation (lambda (old) (+ old 3))
;                  :test (lambda (val) (if (eq 0 (mod val 17)) 0 1)))
;   )
; )

; INPUT MONKEYS:
(defvar all-monkeys
  (list
    ; monkey 0
    (make-monkey :items (list 54 98 50 94 69 62 53 85)
                 :inspections 0
                 :operation (lambda (old) (* old 13))
                 :test (lambda (val) (if (eq 0 (mod val 3)) 2 1)))
    ; monkey 1
    (make-monkey :items (list 71 55 82)
                 :inspections 0
                 :operation (lambda (old) (+ old 2))
                 :test (lambda (val) (if (eq 0 (mod val 13)) 7 2)))
    ; monkey 2
    (make-monkey :items (list 77 73 86 72 87)
                 :inspections 0
                 :operation (lambda (old) (+ old 8))
                 :test (lambda (val) (if (eq 0 (mod val 19)) 4 7)))
    ; monkey 3
    (make-monkey :items (list 97 91)
                 :inspections 0
                 :operation (lambda (old) (+ old 1))
                 :test (lambda (val) (if (eq 0 (mod val 17)) 6 5)))
    ; monkey 4
    (make-monkey :items (list 78 97 51 85 66 63 62)
                 :inspections 0
                 :operation (lambda (old) (* old 17))
                 :test (lambda (val) (if (eq 0 (mod val 5)) 6 3)))
    ; monkey 5
    (make-monkey :items (list 88)
                 :inspections 0
                 :operation (lambda (old) (+ old 3))
                 :test (lambda (val) (if (eq 0 (mod val 7)) 1 0)))
    ; monkey 6
    (make-monkey :items (list 87 57 63 86 87 53)
                 :inspections 0
                 :operation (lambda (old) (* old old))
                 :test (lambda (val) (if (eq 0 (mod val 11)) 5 0)))
    ; monkey 7
    (make-monkey :items (list 73 59 82 65)
                 :inspections 0
                 :operation (lambda (old) (+ old 6))
                 :test (lambda (val) (if (eq 0 (mod val 2)) 4 3)))
  )
)

; simulate a single round with all our monkeys
(defun run-round (monkeys)
  ; iterate through monkeys
  (dotimes (i (length monkeys))
    (let ((m (nth i monkeys)))
      (format t "  monkey ~A:~%" i)
      ; iterate through items
      (loop while (/= 0 (length (monkey-items m))) do
            ; pop item -> handle (affects worry) -> reduce worry -> throw
            (let ((val (pop (monkey-items m))))
              (format t "    starting val: ~A~%" val)
              (setf val (funcall (monkey-operation m) val))
              (format t "    val after op: ~A~%" val)
              (setq val (boredom-factor val))
              (format t "    val becalmed: ~A~%" val)
              (let ((target-idx (funcall (monkey-test m) val)))
                (format t "    tossing ~A to ~A~%" val target-idx)
                (let ((target-monkey (nth target-idx monkeys)))
                  (setf (monkey-items target-monkey) (append (monkey-items target-monkey) (list val)))
                )
              )
            )
            ; increment inspection count
            (incf (monkey-inspections m))
      )
    )
  )
)

; simulate rounds
(dotimes (n 20)
  (format t "Round ~A~%" n)
  (run-round all-monkeys)
)
; print out results
(dotimes (n (length all-monkeys))
  (format t "Monkey ~A inspected ~A items.~%" n (monkey-inspections (nth n all-monkeys)))
)

