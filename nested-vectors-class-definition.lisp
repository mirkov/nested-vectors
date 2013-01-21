(in-package :nested-vectors)

(export '(nested-vector row-count column-count state
	  adjustable-rows-p adjustable-columns-p))

(defclass nested-vector ()
  ((data :accessor raw-data
	    :initarg :raw-data
	    :documentation "Stores the data structure")
   (row-count :accessor row-count
	      :initarg :row-count)
   (column-count :accessor column-count
		 :initarg :column-count)
   (column-specification :accessor column-specification
			 :initarg :column-specification
			 :documentation
"Stores the array type and default element type of each column")
   (state :accessor state
	  :initform nil)
   (adjustable-rows-p :accessor adjustable-rows-p
		      :initarg :adjustable-rows-p
		      :documentation
"The number of columns can be increased.")
   (adjustable-columns-p :accessor adjustable-columns-p
			 :initarg :adjustable-columns-p
			 :documentation
"Column length can be increased.  This flag is incompatible with
foreign-array vectors in the column")
   (default-array-format :accessor default-array-format
     :initarg :default-array-format)
   (default-initial-element :accessor default-initial-element
     :initarg :default-initial-element)
   (default-initial-contents :accessor default-initial-contents
     :initarg :default-initial-contents)
   (default-element-type :accessor default-element-type
     :initarg :default-element-type))
  (:documentation "Nested vector"))

(defmethod print-object ((self nested-vector) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (if (state self)
	(format stream "dimensions ~a by ~a"
		(row-count self) (column-count self))
	(format stream "Uninitialized nested vector"))))


(defmethod describe-object ((self nested-vector) stream)
  (with-slots (state row-count column-count
		     adjustable-rows-p adjustable-columns-p) self
    (if state
	(progn
	  (format stream "~S is a nested vector~%" self)
	  (format stream "It has ~a rows and ~a columns~%" row-count column-count)
	  (format stream "Rows are~:[ not~;~] adjustable~%" adjustable-rows-p)
	  (format stream "Columns are~:[ not~;~] adjustable~%" adjustable-columns-p)
	  (format stream "Column defaults are:~%")
	  (format stream "   Array format: ~a~%" (default-array-format self))
	  (format stream "   Initial-element: ~a~%" (default-initial-element self))
	  (format stream "   Initial-contents: ~a~%" (default-initial-contents self))
	  (format stream "   Element type: ~a~%" (default-element-type self)))
	(format stream "Nested vector is not initialized"))))



