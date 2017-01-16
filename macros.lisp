;; Macros - chapter 7 & 8 of PCL

;; the special operated PROGN
;; executes any number of forms in order
;; and returns the value of the last form.

;; defining  my own when macro
(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))
  
(my-when (= 2 (/ 4 2))
	 (format t "expr 1, ")
	 (format t "expr 2"))

;; defining our own version of my-unless
(defmacro my-unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

;; custom range function
(defun range (end)
  (let ((xs nil) (start 0))
    (dotimes (x (- end start)) (push x xs))
    (nreverse xs)))

;; basic iteration
(dolist (x '(1 2 3)) (print x))

;; basic counting loop
(dotimes (i 4) (print i))

(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))
    
;; a do loop to calculate the 10th fibonacci number
(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))

;; a do loop with a body
;; wait's until teh target time is reached
(defun wait-time (target-time)
  (do ()
      ((> (get-universal-time) target-time))
    (format t "Waiting ~%")
    (sleep 60)))

;; a LOOP version of the above
(defun loopy-wait-time (target-time)
  (loop
       (when (> (get-universal-time) target-time)
	 (return))
     (format t "Waiting ~%")
     (sleep 60)))

;; a LOOP version of range
(defun loopy-range (start end)
  (loop for i from start to (1- end) collecting i))

;; loop - summing
(loop for x from 1 to 10 summing (expt x 2))

;; loop - counting
(loop for x across "the quick brown fox"
     counting (find x "aeiou"))

;; loop - fibonacci
(loop for i below 10
   and a = 0 then b
   and b = 1 then (+ b a)
   finally (return a))
