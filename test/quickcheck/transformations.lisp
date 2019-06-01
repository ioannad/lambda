;;;;test/quickcheck/transformations.lisp

(in-package :lambda.quickcheck)

;; Other syntaxes apart from syntax-standard are not being quickchecked because
;; they require different generators, and because they should only be created by
;; the transformations in transform.lisp.

(defun check-standard->de-bruijn (term free)
  (let ((db-term (standard->de-bruijn term)))
    (is db-term-p db-term)
    (is set-equal
	(db-free-variables db-term)
	free)))

;; standard->common-lisp is checked in test/quickcheck/compiler.lisp

(defun check-transformations (term free)
  (check-standard->de-bruijn term free))
