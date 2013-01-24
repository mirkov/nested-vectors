(in-package :nested-vectors)

(export '(nested-vector row-count column-count
	  operational-p set-to-operational
	  adjustable-rows-p adjustable-columns-p))

(defclass nested-vector ()
  ((data :accessor raw-data
	    :initarg :raw-data
	    :documentation "Stores the data structure")
   (row-count :accessor row-count
	      :initarg :row-count)
   (column-count :accessor column-count
		 :initarg :column-count)
   #+skip(column-specification :accessor column-specification
			 :initarg :column-specification
			 :documentation
"Stores the array type and default element type of each column")
   (state :accessor state
	  :initform nil
	  :documentation "Allowed states are
`defined' and `operational'

The state is `defined' if the nested-vector propestructure ")
   (adjustable-rows-p :accessor adjustable-rows-p
		      :initarg :adjustable-rows-p
		      :documentation
"We can add additional rows.    This flag is incompatible with
foreign-array vectors in any of the columns")
   (adjustable-columns-p :accessor adjustable-columns-p
			 :initarg :adjustable-columns-p
			 :documentation
"We can add additional column length can be increased.")
   #+skip(default-array-format :accessor default-array-format
     :initarg :default-array-format)
   #+skip(default-initial-element :accessor default-initial-element
     :initarg :default-initial-element)
   #+skip(default-initial-contents :accessor default-initial-contents
     :initarg :default-initial-contents)
   #+skip(default-element-type :accessor default-element-type
     :initarg :default-element-type))
  (:documentation "Nested vector"))

(defmethod print-object ((self nested-vector) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "dimensions ~a by ~a, ~a"
	    (row-count self) (column-count self) (state self))))


(defmethod describe-object ((self nested-vector) stream)
  (with-slots (state row-count column-count
		     adjustable-rows-p adjustable-columns-p) self
    (format stream "~S is a ~a nested vector~%" self (state self))
    (format stream "It has ~a rows and ~a columns~%" row-count column-count)
    (format stream "Rows are~:[ not~;~] adjustable~%" adjustable-rows-p)
    (format stream "Columns are~:[ not~;~] adjustable~%" adjustable-columns-p)))



(defgeneric operational-p (nested-vector)
  (:documentation "Return true if the nested-vector is defined and loaded")
  (:method ((self nested-vector))
    (equal 'operational (state self))))

(defgeneric set-to-operational (nested-vector)
  (:documentation "Declare the nested vector to be fully operational")
  (:method ((self nested-vector))
    (setf (state self) 'operational)))
