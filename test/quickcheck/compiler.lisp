;;;;test/quickcheck/compiler-common-lisp.lisp

(in-package :lambda.quickcheck)

(defun check-compiler (terms)
  (loop for term in terms
     do (is equal (type-of (^compile term))
	    'FUNCTION)))
