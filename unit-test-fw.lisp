(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro combine-results (&body testcases)
  (let ((result (gensym)))
    `(let ((,result t))
       ,@(loop for f in testcases collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect
	    `(report-result ,f ',f))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

;; test cases
(deftest test-* () (check (= (* 1 2) 2) (= (* 1 2 3) 6) (= (* -1 -3) 3)))

(deftest test-+ () (check (= (+ 1 3 4) 8) (= (+ 2 4) 6) (= (+ 3 5) 8)))

(deftest test-arithmetic () (combine-results (test-+) (test-*)))
