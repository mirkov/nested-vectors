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

(defun make-vv-spine (column-count &optional adjustable-columns-p)
  "Create the vv spine"
  (make-array column-count :adjustable adjustable-columns-p
	      :fill-pointer column-count))



(define-test make-nested-vector
  "Tests of basic specification"
  (let ((nv (make-nested-vector 3 2)))
    (assert-equal 'defined (state nv))
    (assert-number-equal 3 (row-count nv))
    (assert-number-equal 2 (column-count nv))
    (assert-true (not (adjustable-columns-p nv)))
    (assert-true (not (adjustable-rows-p nv)))
    (assert-number-equal 2 (length (raw-data nv))))
  (let ((nv (make-nested-vector 3 2
				:adjustable-columns-p t)))
    (assert-true (adjustable-columns-p nv)))
  (let ((nv (make-nested-vector 3 2
				:adjustable-rows-p t)))
    (assert-true (adjustable-rows-p nv)))
  (let ((nv (make-nested-vector
	     2 3
	     :initial-contents
	     (list (list 'a 'b)
		   #(1.0 2.0)
		   #m(1.0 2.0)))))
    (assert-equal 1.0 (aref (aref (raw-data nv) 1) 0))))


(defun make-nested-vector (row-count column-count
			   &key
			     initial-contents
			     adjustable-rows-p
			     adjustable-columns-p)
  "Return a nested vector

ROW-COUNT and COLUMN-COUNT specify dimensions

INITIAL-CONTENTS is a list of sequences.  Each sequence's length mush
match ROW-COUNT, and there must be COLUMN-COUNT of them.
"
  (let* ((spine (make-vv-spine column-count adjustable-columns-p))
	 (nv (make-instance 'nested-vector
			    :raw-data spine
			    :row-count row-count
			    :adjustable-rows-p adjustable-rows-p
			    :column-count column-count
			    :adjustable-columns-p adjustable-columns-p)))
    (setf (state nv) 'defined)
    (when initial-contents
      (assert (= (length initial-contents) column-count) ()
	      "Number of sequences:~a in initial contents must match COLUMN-COUNT:~a"
	      (length initial-contents) column-count)
      (loop for sequence in initial-contents
	 for j from 0
	 do (progn
	      (assert (= (sequence-length sequence) row-count) ()
		      "Sequence length:~a, must equal ROW-COUNT:~a"
		      (sequence-length sequence) row-count)
	      (setf (sref spine j) sequence)))
      (setf (state nv) 'operational))
    nv))


