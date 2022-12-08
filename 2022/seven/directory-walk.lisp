#!/usr/bin/sbcl --script
; Walk through a filesystem, build directory information on the way.

; load libraries
(load "~/quicklisp/setup.lisp")
(ql:quickload "str")
(ql:quickload "alexandria")

; main input file
; (defvar filename "test.txt")
(defvar filename "input.txt")

; 'file' data structure
(defstruct file
  name  ; file name
  size  ; file size (bytes, presumably)
)

; 'Folder' data structure
(defstruct folder
  parent  ; parent directory, or NIL if this is the root
  name    ; directory name
  size    ; directory size
  files   ; list of ('file') objects
  folders ; list of ('folder') objects
)

; function to change directory
(defun cd (target current root)
  (if           ; check if target is the root directory and return immediately if so
    (string= target "/")
    root
    (if         ; check if the target is the current folder's parent
      (string= target "..")
      (folder-parent current)
      (find-if (lambda (f) (string= target (folder-name f))) (folder-folders current))
    )
  )
)

; function that recursively calculates the size of a given filesystem
(defun calc-dir-sizes (root)
  ; recursively assign directory sizes
  (defun calc-dir-recursive (current)
    ; get file sizes as starting point
    (let ((size (if (eq NIL (folder-files current)) 0 (apply '+ (mapcar (lambda (f) (file-size f)) (folder-files current))))))
      ; check for stop condition
      (if (folder-folders current)
        ; we need to add the values of our children
        (loop for child in (folder-folders current) do (incf size (calc-dir-recursive child)))
      )
      ; update our size and return it
      (setf (folder-size current) size)
      size
    )
  )
  ; kick off the recursion
  (calc-dir-recursive root)

  ; return a list of all directories, flattened
  (defun flatten-and-simplify-dirs (dir)
    (let ((self (make-file :name (folder-name dir) :size (folder-size dir))))
      (if
        (folder-folders dir)
        (append
          (alexandria:flatten (mapcar #'calc-dir-sizes (folder-folders dir)))
          (list self)
        )
        self
      )
    )
  )

  (flatten-and-simplify-dirs root)
)

; function to add the sizes of all folders below a certain threshold
(defun calc-small-dir-sizes (sizes threshold)
  (let ((res 0))
    (loop for item in sizes
      do (if
           (<= (file-size item) threshold)
           (progn
             (format t "Adding ~A (~A) to ~A~%" (file-size item) (file-name item) res)
             (incf res (file-size item))
           )
         )
    )
    res
  )
)

; function to select the best candidate for deletion
(defun calc-deletion-candidate (root-size sizes total-size required-size)
  ; calculate the amount we need to delete
  (let ((threshold (- required-size (- total-size root-size)))
        (candidate-size total-size)
        (candidate "UNSET"))
    (format t "Must delete ~A worth of files.~%" threshold)
    ; find best candidate
    (loop for item in sizes do
      (if
        (and (> (file-size item) threshold) (< (file-size item) candidate-size))
        (progn
          (setq candidate (file-name item))
          (setq candidate-size (file-size item))
          (format t "Found a new candidate ~A (~A)~%" candidate candidate-size)
        )
      )
    )
    ; return candidate
    (format t "We should remove ~A (~A)~%" candidate candidate-size)
    candidate-size
  )
)

; procedurally walk through the input (output?) file, building up a filesystem
(with-open-file (stream filename)
  (let ((root (make-folder :parent NIL :name "root" :files () :folders ())))
    (do
      ( ; local variable nextline is initialized with the first line
        (nextline (read-line stream nil)  (read-line stream nil))
        (current root)
      )
      ( ; stopping condition and actions
        (null nextline) ; quits when nextline is null, ie eof
        (format t "Finished building filesystem~%")
        (let ((sizes (calc-dir-sizes root)))
          (format t "Sizes: ~A~%" sizes)
          (format t "Cumulative sizes below 100000 (Part A): ~A~%" (calc-small-dir-sizes sizes 100000))
          (format t "Best candidate for deletion:  (Part B): ~A~%" (calc-deletion-candidate (folder-size root) sizes 70000000 30000000))
        )
      )
      ; body of loop; interpret each input line as a command or add to current info
      (let ((words (str:words nextline)))
        (if
          (string= (first words) "$")
          ; process a command
          (progn
            (if (string= (second words) "cd")
              ; change directory
              (setq current (cd (third words) current root))
              ; do nothing - the only other command is 'ls' and we're lazy
            )
          )
          ; process the output of 'ls' (better be!)
          (if
            (string= (first words) "dir")
            ; this is a directory; add it to our current directories
            (progn
              (format t "Adding directory ~A to ~A~%" (second words) (mapcar (lambda (f) (folder-name f)) (folder-folders current)))
              (setf
                (folder-folders current)
                (append
                  (folder-folders current)
                  (list (make-folder :name (second words) :parent current :size 0))
                )
              )
            )
            ; this is a file; add it to our current files
            (setf
              (folder-files current)
              (append
                (folder-files current)
                (list (make-file :size (parse-integer (first words)) :name (second words)))
              )
            )
          )
        ) 
      )
    )
  )
)


