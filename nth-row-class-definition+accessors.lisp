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
  (funcall (row-index-setter self) value))



(defmethod initialize-instance :after ((self nested-vector-row) &key)
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
  (:method ((self nested-vector) i)
    (make-instance 'nested-vector-row
		   :nested-vector self
		   :row-index i)))

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
    (setf (nth-column nv 1) (make-column-vector 2 'array
						:initial-contents '(1.0 2.0)))
    (setf (nth-column nv 2) (make-column-vector 2 'grid:foreign-array
						:initial-contents '(3.0 4.0)))
    (let ((row (nth-row  nv 1)))
      (assert-equal 'b (vrref row 0))
      (assert-equal 2.0 (vrref row 1))
      (setf (vrref row 2) 15.0) ; ((a b) (1 2) (3 15))
      (assert-equal 15.0 (vrref row 2)) 
      (setf (row-index row) 0) 
      (assert-equal 'a (vrref row 0))
      (setf (vrref row 0) 'c) ; ((c b) (1 2) (3 15))
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
  
