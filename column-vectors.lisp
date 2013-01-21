(in-package :nested-vectors)

(export '(make-column-vector))

(defgeneric make-column-vector (row-count array-type &key)
  (:documentation "Return a vector of length ROW-COUNT

ARRAY-TYPE is one of 'cl:array or 'grid:foreign-array

Keywords can be used to specify initial contents.  

cl-arrays can be made adjustable")
  (:method (row-count (array-type (eql 'cl:array))
	    &key (element-type t)
	      adjustable
	      (initial-element nil initial-element-supplied-p)
	      (initial-contents nil initial-contents-supplied-p))
      (apply #'make-array row-count :element-type element-type
	     :adjustable adjustable
	     (cond
	       (initial-element-supplied-p (list :initial-element initial-element))
	       (initial-contents-supplied-p (list :initial-contents initial-contents)))))
  (:method (row-count (array-type (eql 'cl:list))
	    &key 
	      initial-element
	      (initial-contents nil initial-contents-supplied-p)
	      &allow-other-keys)
    (if initial-contents-supplied-p
	(progn
	  (assert (= (length initial-contents)
		     row-count) ()
		     "Initial contents length~a must equal row-count:~a"
		     (length initial-contents) row-count)
	  (copy-seq initial-contents))
	(make-list row-count :initial-element initial-element))))

(defmethod make-column-vector (row-count (array-type (eql 'grid:foreign-array))
			       &key (element-type 'double-float) 
			 	(initial-element 0.0 initial-element-supplied-p)
			 	(initial-contents nil initial-contents-supplied-p)
				&allow-other-keys)
  (apply #'grid:make-foreign-array element-type :dimensions `(,row-count)
	     (cond
	       (initial-element-supplied-p (list :initial-element initial-element))
	       (initial-contents-supplied-p (list :initial-contents initial-contents)))))


