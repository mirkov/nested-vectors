(in-package :nested-vectors)

(export '(add-column add-row))

(defgeneric add-column (column nested-vector)
  (:documentation 
"Push a new column onto a nested-vector")
  (:method :before (sequence (nested-vector nested-vector))
	   (assert (adjustable-columns-p nested-vector) ()
		   "Cannot push columns on nested-vector:~a" nested-vector)
	   (assert (= (sequence-length sequence) (row-count nested-vector)) ()
		   "Sequence length:~a must equal ROW-COUNT:~a"
		   (sequence-length sequence) (row-count nested-vector)))
  (:method (sequence (nested-vector nested-vector))
    (vector-push-extend sequence (raw-data nested-vector))
    (incf (column-count nested-vector))))

(define-test add-column
  (let ((nv (make-nested-vector 3 0 :adjustable-columns-p t)))
    (add-column '(a b c) nv)
    (assert-equal '(a b c) (nth-column nv 0))
    (assert-equal 1 (column-count nv))
    (add-column #(1 2 3) nv)
    (assert-numerical-equal #(1 2 3) (nth-column nv 1))
    (assert-equal 2 (column-count nv))))



(defgeneric increase-column-count (nested-vector additional-column-count)
  (:documentation 
"Increase the number of columns and sets the state to DECLARED")
  (:method ((self nested-vector) delta-j)
    (setf (raw-data nested-vector)
	  (adjust-array (raw-data nested-vector)
			(+ (column-count nested-vector)
			   delta-j))
	  (state nested-vector) 'declared)))

(defgeneric add-row (row nested-vector)
  (:documentation 
"Push a new row onto a nested-vector")
  (:method :before (sequence (nested-vector nested-vector))
	   (assert (equal 'operational (state nested-vector)) ()
		   "Nested vector state:~a must be OPERATIONAL"
		   (state nested-vector))
	   (assert (adjustable-rows-p nested-vector) ()
		   "Cannot push row on nested-vector:~a" nested-vector)
	   (assert (= (sequence-length sequence) (column-count nested-vector)) ()
		   "Sequence length:~a must equal COLUMN-COUNT:~a"
		   (sequence-length sequence) (column-count nested-vector)))
  (:method ((list list) (nested-vector nested-vector))
    (let ((spine (raw-data nested-vector)))
      (loop for element in list
	   for j from 0
	   do (setf (aref spine j)
		    (extend-sequence (aref spine j) element)))))
  (:method ((vector vector) (nested-vector nested-vector))
    (let ((spine (raw-data nested-vector)))
      (loop for element across vector
	 for j from 0
	 do (setf (aref spine j) (extend-sequence (aref spine j) element)))))
  (:method :after (sequence (nested-vector nested-vector))
	   (declare (ignore sequence))
	   (incf (row-count nested-vector))))


(define-test add-row
  (let ((nv (make-nested-vector
	     3 2 :adjustable-columns-p t
	     :adjustable-rows-p t
	     :initial-contents
	     (list (list 'a 'b 'c)
		   (make-array 3
			       :initial-contents '(1 2 3)
			       :adjustable t
			       :fill-pointer 3)))))
    (print (raw-data nv))
    (add-row '(d 4) nv)
    (print (raw-data nv))
    (assert-equal '(d 4) (row-contents (nth-row nv 3)))
    (assert-equal 4 (row-count nv))
    (add-column #(10 20 30 40) nv)
    (print (raw-data nv))
    (assert-numerical-equal #(1 2 3 4) (nth-column nv 1))
    (assert-numerical-equal #(10 20 30 40) (nth-column nv 2))
    (assert-equal 3 (column-count nv))))
