(in-package :nested-vectors)

(export '(nested-vector-row nth-row row-index row-length vrref))

(defclass nested-vector-row ()
  ((nested-vector :accessor nested-vector
		  :initarg :nested-vector
		  :documentation
		  "Nested vector whose row we are accessing")
   (row-index :accessor row-index
	      :initarg :row-index
	      :documentation
	      "Row index")
   (row-index-setter :accessor row-index-setter)
   (value-reader :accessor value-reader
		 ;;:initarg :value-reader
		 :documentation
"Function of one argument, column-index, that is used to access row element

The optional second argument is a flag.  When set to true, the
argument is used to change the row-index.

This index change affects both reading and writing.
")
   (value-writer :accessor value-writer
		 ;;:initarg :value-writer
		 :documentation
"Function of one argument, column-index, that is used to access row element

The optional second argument is a flag.  When set to true, the
argument is used to change the row-index.

This index change affects both reading and writing.
"))
  (:documentation
   "Object that encapsulates the facilities for reading and writing row elements

Check documentation on VALUE-READER and VALUE-WRITER slots for details of usage

"))

(defmethod print-object ((self nested-vector-row) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "row ~a" (row-index self))))

(defmethod describe-object ((self nested-vector-row) stream)
  (format stream "~a-th row of nested vector ~a~%" (row-index self)
	  (nested-vector self)))


(defmethod (setf row-index) :before (value (self nested-vector-row))
  (assert (< value (row-count (nested-vector self))) ()
	  "Row index: ~a must be less than row count: ~a"
	  value (row-count (nested-vector self)))
  (assert (>= value 0) ()
	  "Row index: ~a must be greater then or equal to 0" value))

(defmethod (setf row-index) :after (value (self nested-vector-row))
  "After setting the slot value, I also set the row-index value in the
closure by calling the index setter function"
  (funcall (row-index-setter self) value))



(defmethod initialize-instance :after ((self nested-vector-row) &key)
  "We establish the closure with the nested vector and the row index.
We then define the functions for retreiving and setting the
nested-vector values, and changing the row inex.  These functions are
stored in the nested-row slots"
  (let ((nested-vector (nested-vector self))
	(i (row-index self)))
    (setf (value-reader self) (lambda (j)
				(vvref nested-vector i j))
	  (value-writer self) (lambda (value j)
				(setf (vvref nested-vector i j)
				      value))
	  (row-index-setter self) (lambda (index)
				    (setf i index)))))

(defgeneric nth-row (nested-vector row-index)
  (:documentation
"Return a nested-vector-row object that enables reading and writing
to elements of row I")
  (:method :before ((self nested-vector) i)
	   (declare (ignore i))
	   (assert (equal 'operational (state self)) ()
		   "Nested vector state:~a must be OPERATIONAL"
		   (state self)))
  (:method ((self nested-vector) i)
    (make-instance 'nested-vector-row
		   :nested-vector self
		   :row-index i)))

(defgeneric (setf nth-row) (sequence nested-vector row-index)
  (:documentation "Set nth-row of neste-vector

SEQUENCE contains the rows new values.  Its length must
equal (column-count nested-vector).  SEQUENCE can be a list, a vector
or a grid vector.")
  ;; these methods do not depend at all on nested-vector-row.  Does
  ;; that point to an incosistency in the coding logic?
  (:method :before (sequence (nested-vector nested-vector) row-index)
	   (declare (ignore sequence))
	   (assert (equal 'operational (state nested-vector)) ()
		   "Nested vector state:~a must be OPERATIONAL"
		   (state nested-vector))
	   (assert (< row-index (row-count nested-vector)) ()
		   "Row index: ~a must be less than row count: ~a"
		   row-index (row-count nested-vector))
	   (assert (>= row-index 0) ()
		   "Row index: ~a must be greater then or equal to 0" row-index))
  (:method ((list list) (nested-vector nested-vector) i)
    (loop for element in list
	 for j from 0
	 do (setf (vvref nested-vector i j) element)))
  (:method ((vector vector) (nested-vector nested-vector) i)
    (loop for element across vector
	 for j from 0
	 do (setf (vvref nested-vector i j) element)))
  (:method ((vector grid:mvector) (nested-vector nested-vector) i)
    (iter:iter
      (iter:for element :in-vector vector)
      (iter:for j :vector-element-index vector)
      (setf (vvref nested-vector i j) element))))

(defgeneric row-length (nested-vector-row)
  (:documentation 
"Return length of nested vector row")
  (:method ((self nested-vector-row))
    (column-count (nested-vector self))))

(defgeneric vrref (nested-vector-row j)
  (:documentation
   "Retrieve element in j-th column of nested vector row")
  (:method ((self nested-vector-row) j)
    (funcall (value-reader self) j)))

(defgeneric (setf vrref) (value nested-vector-row j)
  (:documentation
   "Set element in j-th column of nested vector row")
  (:method (value (self nested-vector-row) j)
    (funcall (value-writer self) value j)))

(define-test nth-row
  "Test retriving and setting row values.  Then test setting the row,
and also incrementing and decrementing the row"
  (let ((nv (make-nested-vector 2 3)))
    ;; we start with ((a b) (1 2) (3 4))
    (setf (nth-column nv 0) (list 'a 'b))
    (setf (nth-column nv 1) (make-sequence-1 'array 2
					     :initial-contents '(1.0 2.0)))
    (setf (nth-column nv 2) (make-sequence-1 'grid:foreign-array 2
					     :initial-contents '(3.0 4.0)))
    (set-to-operational nv)
    (let ((row (nth-row  nv 1)))
      (assert-equal 'b (vrref row 0))
      (assert-equal 2.0 (vrref row 1))
      (setf (vrref row 2) 15.0)		; ((a b) (1 2) (3 15))
      (assert-equal 15.0 (vrref row 2)) 
      (setf (row-index row) 0) 
      (assert-equal 'a (vrref row 0))
      (setf (vrref row 0) 'c)		; ((c b) (1 2) (3 15))
      (assert-equal 'c (vrref row 0)) 
      (assert-equal 0 (row-index row))
      (incf (row-index row))
      (assert-equal 'b (vrref row 0))
      (assert-equal 2.0 (vrref row 1))
      (assert-equal 15.0 (vrref row 2))
      (decf (row-index row))
      (assert-equal 'c (vrref row 0))
      (assert-equal 1.0 (vrref row 1))
      (assert-equal 3.0 (vrref row 2)))))

(define-test setf-nth-row
  "Test retriving and setting row values.  Then test setting the row,
and also incrementing and decrementing the row"
  (let ((nv (make-nested-vector 2 3
				:initial-contents `((a b)
						    #(1 2)
						    #m(1.0 2.0)))))
    (setf (nth-row nv 0) '(3 2 2.0))
    (let ((row (nth-row  nv 0)))
      (assert-equal 3 (vrref row 0))
      (assert-equal 2 (vrref row 1))
      (assert-equal 2.0 (vrref row 2)))))

(defgeneric row-contents (nested-vector-row)
  (:documentation "Return row contents as list")
  (:method ((self nested-vector-row))
    (iter:iter (iter:for element :in-nv-row-element self)
	       (iter:collect element))))

(defgeneric nested-vector-row-contents (nested-vector i)
  (:documentation "Return row contents of a nested vector")
  (:method :before ((nested-vector nested-vector) row-index)
	   (assert (equal 'operational (state nested-vector)) ()
		   "Nested vector state:~a must be OPERATIONAL"
		   (state nested-vector))
	   (assert (< row-index (row-count nested-vector)) ()
		   "Row index: ~a must be less than row count: ~a"
		   row-index (row-count nested-vector))
	   (assert (>= row-index 0) ()
		   "Row index: ~a must be greater then or equal to 0" row-index))
  (:method ((self nested-vector) i)
    (loop for j below (column-count self)
       collect (vvref (raw-data self) i j))))
