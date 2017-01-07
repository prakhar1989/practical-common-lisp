;; global variable
(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title
	:artist artist
	:rating rating
	:ripped ripped))

(defun add-record (cd)
  (push cd *db*))

(defun show-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or
    (parse-integer
     (prompt-read "Rating") :junk-allowed t)
    0)
   (y-or-n-p "Ripped")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another?")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))

(defun clear-db () (setf *db* nil))

(defun read-file (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax (read in))))

(defun load-db (filename)
  (if (or (not *db*)
	  (y-or-n-p "Override data in memory?"))
      (setf *db* (read-file filename))))

(defun select-by-prop (prop query)
  (remove-if-not
   #'(lambda (cd)
       (equal (getf cd prop) query))
   *db*))

;; (select (where :rating 4))
(defun where (&key title artist rating
		(ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; (update (where :rating 4) :rating 11)
(defun update (selector-fn &key title artist rating
			     (ripped nil ripped-p))
  (setf *db*
	(mapcar #'(lambda (row)
		    (when (funcall selector-fn row)
		      (if title (setf (getf row :title) title))
		      (if artist   (setf (getf row :artist) artist))
		      (if rating   (setf (getf row :rating) rating))
		      (if ripped-p (setf (getf row :ripped) ripped)))
		    row)
		*db*)))

;; (delete-rows (where :rating 5))
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
