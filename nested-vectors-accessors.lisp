(in-package :nested-vectors)

(export '(nth-column vvref))

(defgeneric nth-column (nested-vector j)
  (:documentation
   "Return the nth-column object")
  (:method ((self nested-vector) j)
    (aref (raw-data self) j)))

(define-test nth-column
  "Test setting and retreiving columns"
  (let ((nv
	 (make-nested-vector
	  2 3)))
    (setf (nth-column nv 0) (list 'a 'b))
    (setf (nth-column nv 1) (make-sequence-1 'array 2
						:initial-contents '(1.0 2.0)))
    (setf (nth-column nv 2) (make-sequence-1 'grid:foreign-array 2
						:initial-contents '(3.0 4.0)))
    (assert-equal '(a b) (nth-column nv 0))
    (assert-numerical-equal #(1.0 2.0) (nth-column nv 1))
    (assert-numerical-equal #(3.0 4.0) (grid:copy-to
					(nth-column nv 2)))))


(defgeneric (setf nth-column) (column nested-vector j)
  (:documentation
   "Return the column object")
  (:method (column (self nested-vector) j)
    (setf (aref (raw-data self) j) column))
  (:method :before (value (self nested-vector) j)
	   (declare (ignore value))
	   (assert (< j (column-count self)) ()
"Column index:~a, is greater than the nested-vector column count:~a"
j (column-count self)))
  (:method :before ((list list) (self nested-vector) j)
	   (declare (ignore j))
	   (assert (= (length list) (row-count self)) ()
		   "List length:~a, must match row count:~a"
		   (length list) (row-count self)))
  (:method :before ((vector array) (self nested-vector) j)
	   (declare (ignore j))
	   (let ((dimensions (array-dimensions vector)))
	     (assert (not (cdr dimensions)) ()
		     "Vector rank:~a, must be 1" (length dimensions))
	     (assert (= (car dimensions) (row-count self)) ()
		     "Vector length:~a, must match row count:~a"
		     (car dimensions) (row-count self)))
	   (assert (equal (adjustable-array-p vector)
			  (adjustable-rows-p self)) ()))
  (:method :before ((vector grid:vector-double-float) (self nested-vector) j)
	   (declare (ignore j))
	   (assert (not (adjustable-rows-p self)) ()
		   "Nested vector cannot have rows columns")
	   (let ((dimensions (grid:dimensions vector)))
	     (assert (not (cdr dimensions)) ()
		     "Vector rank:~a, must be 1" (length dimensions))
	     (assert (= (car dimensions) (row-count self)) ()
		     "Vector length:~a, must match row count:~a"
		     (car dimensions) (row-count self)))))




(defgeneric cref (column-vector i)
  (:documentation
"Access i-th element of column vector")
  (:method ((column-vector list) i)
    (nth i column-vector))
  (:method ((column-vector array) i)
    (aref column-vector i))
  (:method ((column-vector grid:foreign-array) i)
    (grid:aref column-vector i)))


(defgeneric (setf cref) (value column-vector i)
  (:documentation
"Set i-th element of column vector")
  (:method (value (column-vector list) i)
    (setf (nth i column-vector) value))
  (:method (value (column-vector array) i)
    (setf (aref column-vector i) value))
  (:method (value (column-vector grid:foreign-array) i)
    (setf (grid:aref column-vector i) value)))

(defgeneric vvref (nested-vector i j)
  (:documentation
   "Access element of nested vector in row i and column j")
  (:method :before ((self nested-vector) i j)
	   (declare (ignore i j))
	   (assert (equal 'operational (state self)) ()
		   "Nested vector state:~a must be OPERATIONAL"
		   (state self)))
  (:method ((self nested-vector) i j)
    (cref (aref (raw-data self) j) i)))

(defgeneric (setf vvref) (value nested-vector i j)
  (:documentation
"Set row-element i, column-element j to value")
  (:method :before (value (self nested-vector) i j)
	   (declare (ignore value i j))
	   (assert (equal 'operational (state self)) ()
		   "Nested vector state:~a must be OPERATIONAL"
		   (state self)))
  (:method (value (self nested-vector) i j)
    (setf (cref (aref (raw-data self) j) i) value)))




