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

;; building a do-prime macro
(defun primep (number)
  (when (> number 1)
    (loop for x from 2 to (isqrt number)
       never (zerop (mod number x)))))

(defun next-prime (number)
  (loop for n from number
     when (primep n)
     return n))

;; goal - the code below doesn't work yet
(do-primes (p 0 19)
  (format t "~d " p))

;; accomplishing the same using a do construct
;; serves as a guiding tool for macroexpand
(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d " p))

;; time to define the macro
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

;; now this works
(do-primes (p 0 19)
  (format t "~d " p))

;; and its associated macroexpansion
(macroexpand '(do-primes (p 0 19) (format t "~d " p)))

;; fixing the first leak - multiple evalutions
;; of end
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	(ending-value ,end))
       ((> ,var ending-value))
     ,@body))

;; fixing the second leak - clashing symbols
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))

;; a macro for macros
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
	  (,ending-value-name ,end))
	 ((> ,var ,ending-value-name))
       ,@body)))
