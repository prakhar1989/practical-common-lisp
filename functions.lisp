(defun multiple-prints ()
  "each expression is evaluated, last one is returned"
  (format t "hello, ")
  (format t "can you hear me ")
  (format t "i'm in california ")
  (format t "dreaming about how ")
  (format t "we used to be"))

(defun verbose-sum (x y)
  "sum two numbers after printing a msg"
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

(defun foo (a b &optional c d)
  "c and d are nil if not provided"
  (list a b c d))

(defun foo-default (a b &optional (c 10) (d 20))
  "c and d now have default values"
  (list a b c d))

(defun foo-supplied (a b &optional (c 3 c-supplied-p))
  "c has a default value of 3, and
  c-supplied-p is T if c has been provided"
  (list a b c c-supplied-p))

(defun adder (&rest numbers)
  (reduce #'+ numbers))
			   
;; keyword arguments are not positional.
;; they also support default and *-supplied-p
;; type of arguments
(defun foo-kwargs (&key a b c)
  "can be called like -> (foo-kwargs :b 1 :a 2 :c 5)"
  (list a b c))

(defun foo-fancy (&key
		    (a 0) ; default arg
		    (b 0 b-supplied-p) ; check if provided
		    (c (+ a b))) ; composite arg
  (list a b b-supplied-p c))

(defun foo-alias (&key
		    ((:apple a))
		    ((:box b) 0)
		    ((:charlie c) 0 c-supplied-p))
  "aliases for the caller: (foo-alias :apple 10)"
  (list a b c c-supplied-p))

(defun greater-than (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
	(return-from greater-than (list i j))))))

(defun plot (fn min max step)
  (loop for i from min to max by step do
       (loop repeat (funcall fn i) do
	    (format t "*"))
       (format t "~%")))

(defun apply-adder (&rest nums)
  (apply #'+ nums))

(plot (lambda (x) (* x x)) 0 5 1/2)
