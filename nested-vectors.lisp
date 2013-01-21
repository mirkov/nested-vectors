;;;; nested-vectors.lisp

(in-package #:nested-vectors)



(defun ensure-list (arg)
  (typecase arg
    (atom (list arg))
    (t arg)))

(define-test make-vv-spine
  (multiple-value-bind (vec dim adjust-p)
      (make-vv-spine 2)
    (assert-true (typep vec 'vector))
    (assert-equal 2 dim)
    (assert-true (not adjust-p)))
  (multiple-value-bind (vec dim adjust-p)
      (apply #'make-vv-spine '(2 :adjustable-rows-p t))
    (assert-true (typep vec 'vector))
    (assert-equal 2 dim)
    (assert-true adjust-p)))

(defun make-vv-spine (column-count &key adjustable)
  "Create the vv spine"
    (values
     (make-array column-count :adjustable adjustable)
     column-count adjustable))

(defgeneric make-column-vector (row-count array-type &key)
  (:documentation "Return a vector of length ROW-COUNT

ARRAY-TYPE is one of 'cl:array or 'grid:foreign-array

Keywords can be used to specify initial contents.  

cl-arrays can be made adjustable")
  (:method (row-count (array-type (eql 'cl:array))
	    &key (element-type t)
	      adjustable initial-element initial-contents)
  (if initial-element
    (make-array row-count :element-type element-type
		:adjustable adjustable
		:initial-element initial-element)
    (make-array row-count :element-type element-type
		:adjustable adjustable
		:initial-contents initial-contents)))
  (:method (row-count (array-type (eql 'cl:list))
	    &key initial-element initial-contents
	      &allow-other-keys)
  (if initial-element
    (make-list row-count :initial-element initial-element)
    (copy-tree initial-contents))))

(defmethod make-column-vector(row-count (array-type (eql 'grid:foreign-array))
			      &key (element-type 'double-float) 
				initial-element initial-contents
				&allow-other-keys)
  (if initial-element
      (grid:make-foreign-array element-type :dimensions `(,row-count)
			   :initial-element initial-element)
      (grid:make-foreign-array element-type :dimensions `(,row-count)
			 :initial-contents initial-contents)))
      




(defun make-nested-vector (row-specification row-count
			   &key default-column-specifications
			     column-specifications)
  "Return a nested vector with dimensions specified by
  row-specification and column-specification

The specifications are either a number or a list.  When the
specification is a number, the dimension is fixed.

When the dimension is a list, it is of format (number :keyword
value :keyword value ...).

For row-specification, the only keyword accepted is :adjustable.  If
t, the row length can be increased.

The column specification accepts additional keywords:
- :element-type
- :array-type

:element-type specifies the type stored in the array.  It is used for
CL's optimization of array storage.  nested-vectors performs no
checking on the actual stored elements

:array-type specifies the type stored column vectors.  It can be
either 'cl:array or 'grid:foreign-array.

:array-type of 'grid:foreign-array is incompatible with :adjustable t

When the nested-vector adds another column, and it's rows have been
declared and initialized, the new column is also initialized."
  (multiple-value-bind (spine column-count adjustable-rows-p)
      (apply #'make-vv-spine (ensure-list row-specification))
    (unless column-specifications
      (setf column-specifications (make-list column-count)))
    (destructuring-bind (&key (default-vector-type 'cl:array)
			      default-initial-element
			      default-initial-contents
			      adjustable-columns-p
			      (default-element-type t))
	default-column-specifications
      (assert (not (and adjustable-columns-p
			(eq default-vector-type 'grid:foreign-array)))
	      () 
	      "ADJUSTABLE-ROWS-P and GRID:FOREIGN-ARRAY default vector type are incompatible")
      (loop for j below column-count
	 for column-specification in column-specifications
	 do (destructuring-bind
		  (&key (vector-type default-vector-type)
			(element-type default-element-type)
			(initial-element default-initial-element)
			(initial-contents default-initial-contents)
			(adjustable adjustable-columns-p))
		column-specification
	      (setf (aref spine j)
		    (make-column-vector row-count vector-type
					:element-type element-type
					:initial-element initial-element
					:initial-contents initial-contents
					:adjustable adjustable))))
      (let ((nv (make-instance 'nested-vector
			       :raw-data spine
			       :row-count row-count
			       :adjustable-rows-p adjustable-rows-p
			       :column-count column-count
			       :default-array-format default-vector-type
			       :default-initial-element default-initial-element
			       :default-initial-contents default-initial-contents
			       :default-element-type default-element-type
			       :adjustable-columns-p adjustable-columns-p)))

	(setf (state nv) 'initialized)
	nv))))


