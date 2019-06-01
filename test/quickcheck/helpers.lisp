;;;;test/quickcheck/helpers.lisp

(in-package :lambda.quickcheck)

;;; Helper functions, to help write the cl-quickcheck tests.

(defun assign-zip (variables terms)
  (loop
     for variable in variables
     for term in terms
     collect (cons variable term)))

(defun set-equal (list-1 list-2)
  (and (null (set-difference list-1 list-2))
       (null (set-difference list-2 list-1))))

(defun random-index (list)
  (random (length list)))

(defun random-element (list)
  (nth (random-index list) list))

(defun remove-nth-element (sequence n)
  (append (subseq sequence 0 n)
	  (subseq sequence (1+ n))))

(defun random-subseq (sequence size &aux (seq sequence))
  (if (or (null size)
	  (null sequence))
      '()
      (progn (loop for i from 0 to size
		do (setf seq
			 (let ((n (random-index seq)))
			   (remove-nth-element seq n))))
	     seq)))


