(in-package :iterate)

;;; Define clauses to iterate over nested vector rows and columns
(defclause-sequence in-nv-row row-index-of-nv
  :access-fn
  '(lambda (nested-vector index)
    (nested-vectors:nth-row nested-vector index))
  :size-fn
  '(lambda (nested-vector)
    (nested-vectors:row-count nested-vector))
  :element-type 'nested-vector-row
  :sequence-type 'nested-vector
  :element-doc-string "Accessor to nested-vector rows"
  :index-doc-string "Row index")

(iter:defclause-sequence in-nv-column column-index-of-nv
  :access-fn
  '(lambda (nested-vector index)
    (nested-vectors:nth-column nested-vector index))
  :size-fn
  '(lambda (nested-vector)
    (nested-vectors:column-count nested-vector))
  :element-type 'sequence
  :sequence-type 'nested-vector
  :element-doc-string "Accessor to nested-vector columns"
  :index-doc-string "Column index")
  


#|
(let ((nv (nested-vectors:make-nested-vector 2 3)))
  (setf (nested-vectors:nth-column nv 0)
	(nested-vectors:make-column-vector 2 'list :initial-contents '(a b))
	(nested-vectors:nth-column nv 1)
	(nested-vectors:make-column-vector 2 'array :initial-contents '(1.0 2.0))
	(nested-vectors:nth-column nv 2)
	(nested-vectors:make-column-vector 2 'grid:foreign-array :initial-contents '(3.0 4.0)))
  (iter:iter (iter:for r :in-nv-row nv)
	(princ r) (princ "
"))
  (iter:iter (iter:for ri :row-index-of-nv nv)
	(princ ri) (princ "
"))
  (iter:iter (iter:for c :in-nv-column nv)
	(princ c) (princ "
"))
  (iter:iter (iter:for ci :column-index-of-nv nv)
	(princ ci) (princ "
")))
|#


(defclause-sequence in-nv-row-element row-index-of-nv-row
  :access-fn
  '(lambda (nested-vector-row index)
    (nested-vectors:vrref nested-vector-row index))
  :size-fn
  '(lambda (nested-vector-row)
    (nested-vectors:row-length nested-vector-row))
  :element-type t
  :sequence-type 'nested-vector-row
  :element-doc-string "Accessor to elements of a nested-vector row"
  :index-doc-string "Index of elements of a nested vector row")

#|
(let ((nv (nested-vectors:make-nested-vector 2 3)))
  (setf (nested-vectors:nth-column nv 0)
	(nested-vectors:make-column-vector 2 'list :initial-contents '(a b))
	(nested-vectors:nth-column nv 1)
	(nested-vectors:make-column-vector 2 'array :initial-contents '(1.0 2.0))
	(nested-vectors:nth-column nv 2)
	(nested-vectors:make-column-vector 2 'grid:foreign-array :initial-contents '(3.0 4.0)))
  (let ((row (nested-vectors:nth-row nv 0)))
    (iter:iter (iter:for v :in-nv-row-element row)
	       (princ v) (princ " "))
    (incf (nested-vectors:row-index row))
    (princ "
")
    (iter:iter (iter:for v :in-nv-row-element row)
	       (princ v) (princ " "))
    (decf (nested-vectors:row-index row))
    (princ "
")
    (iter:iter (iter:for v :in-nv-row-element row)
	       (princ v) (princ " "))))

|#
