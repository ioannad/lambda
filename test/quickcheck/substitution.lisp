;;;;test/quickcheck/substitution.lisp

(in-package lambda.quickcheck)

(defun check-^substitute (term free)
  (let ((free-variable (random-element free))
	(gensym (gensym)))
    (let* ((substituted (^substitute term
				     (assign free-variable gensym)))
	   (new-free (^free-variables substituted)))
      (is ^term-p substituted)
      (isnt member free-variable new-free)
      (is member gensym new-free))))

(defun check-^substitute-environment (term free bound depth)
  (let* ((random-size (if free
			  (random (length free))
			  0))
	 (vars (union (random-subseq free random-size)
		      (some-^variables bound)))
	 (terms (loop for i from 1 to random-size
		   collect (a-^term depth)))
	 (assignments (assign-zip vars terms)))
    (let ((substituted (^substitute-environment term assignments)))
      (is ^term-p substituted))))

;; in test/quickcheck/all.lisp
(defun check-substitution (term free bound depth)
  (check-^substitute term free)
  (check-^substitute-environment term free bound depth))
