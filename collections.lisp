;; creating a set of vectors
(vector 1 2)
(vector 1 2 3); same as #(1 2 3)

;; this creates a fixed size vector (or array)
(make-array 5 :initial-element nil)

;; this creates a resizable vector
;; it hold only elements that have been pushed
;; but can only store 5 elements
(defvar *x* (make-array 5 :fill-pointer 0))
(dotimes (n 10) (vector-push n *x*))
(= (length *x*) 5) ; true

;; to create a dynamically sized vector
;; use adjustable
(setf *x* (make-array 5 :fill-pointer 0 :adjustable t))
(dotimes (n 10) (vector-push-extend n *x*))
(= (length *x*) 10)

;; sequences
(defparameter *y* (vector 1 2 3))
(= (elt *y* 0) 1) ; true
(setf (elt *y* 0) 2)
(= (elt *y* 0) 1)

;; find
(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first) ; => (c 30)

;; higher order variants
(count-if #'evenp #(1 2 3 4 5))
(position-if #'digit-char-p "abcd0001")
(remove-if-not #'(lambda (x) (char= (elt x 0) #\f))
	       #("foo" "bar" "baz" "foom"))

(remove-duplicates #(1 2 1 2 3 1 2 3 4))

(concatenate 'vector #(1 2 3) '(4 5 6))

(defvar my-seq '(6 4 3 1 5 10 -3))
(sort my-seq #'<) ; doesn't mutate the list
(setf my-seq (sort my-seq #'<)) ; mutates the list

;; subsequences
(subseq "foobarbaz" 3)  ; => barbaz
(subseq "foobarbaz" 3 5); => "bar"

;; sequence predicates
(every #'evenp #(1 2 3 4 5))
(some #'evenp #(1 2 3 4 5))
(notany #'evenp #(1 2 3 4 5))
(notevery #'evenp #(1 2 3 4 5))

;; sequence mapping
(map 'vector #'* #(1 2 3) #(10 9 8))
(map-into my-seq #'+
	  '(1 2 3 10 10)
	  '(4 5 6 10 10)
	  '(7 8 9 10 10))

(reduce #'+ #(1 2 3 4 5 6))

;; hash tables
(defparameter *h* (make-hash-table))
(gethash :name *h*); nil
(setf (gethash :name *h*) "prakhar")
(gethash :name *h*)
(setf (gethash :middle-name *h*) nil)
(setf (gethash :sex *h*) "male")
(setf (gethash :job *h*) "programmer")

(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
	(format nil "value ~a actually present." value)
	(format nil "value ~a because key not found." value))))
  
(show-value :age *h*)
(show-value :name *h*)
(show-value :middle-name *h*)
