;;;; nested-vectors.lisp

(in-package #:nested-vectors)

(export '(make-nested-vector))

(defun ensure-list (arg)
  (typecase arg
    (atom (list arg))
    (t arg)))

(define-test make-vv-spine
  (let ((vec (make-vv-spine 2)))
    (assert-true (typep vec 'vector))
    (assert-equal 2 (array-dimension vec 0))))

(defun make-vv-spine (column-count &optional adjustable-rows-p)
  "Create the vv spine"
  (make-array column-count :adjustable adjustable-rows-p))



(define-test make-nested-vector
  "Tests of basic specification"
  (let ((nv (make-nested-vector 3 2)))
    (assert-equal 'defined (state nv))
    (assert-number-equal 3 (row-count nv))
    (assert-number-equal 2 (column-count nv))
    (assert-true (not (adjustable-rows-p nv)))
    (assert-true (not (adjustable-columns-p nv)))
    (assert-number-equal 2 (length (raw-data nv))))
  (let ((nv (make-nested-vector 3 2
				:adjustable-rows-p t)))
    (assert-true (adjustable-rows-p nv)))
  (let ((nv (make-nested-vector 3 2
				:adjustable-columns-p t)))
    (assert-true (adjustable-columns-p nv))))


(defun make-nested-vector (row-count column-count
			   &key
			     adjustable-rows-p
			     adjustable-columns-p
			     column-specifications)
  "Return a specified, but empty nested vector"
  (let ((spine (make-vv-spine column-count adjustable-rows-p)))
    (unless column-specifications
      (setf column-specifications (make-list column-count)))
    (let ((nv (make-instance 'nested-vector
			     :raw-data spine
			     :row-count row-count
			     :adjustable-rows-p adjustable-rows-p
			     :column-count column-count
			     :adjustable-columns-p adjustable-columns-p)))
      (setf (state nv) 'defined)
      nv)))


