(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
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

;; sample test case
(defun test-* ()
  (check
    (= (* 1 2) 2)
    (= (- 1 2 3) 6)
    (= (* -1 -3) 3)))
