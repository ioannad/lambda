;;;;test/quickcheck/renames-equality.lisp

(in-package :lambda.quickcheck)

(defun check-renames-equality (var body)
  (let ((abstraction (list '^ var body)))
    (^eq (α-convert var body)
	 abstraction)))

(defun check-renamed-term (term free)
  (let* ((old-free (random-element free))
	 (new-free (gensym))
	 (renamed (rename term old-free new-free)))
    (is ^term-p renamed)
    (is set-equal
	(union (set-difference free (list old-free))
	       (list new-free))
	(^free-variables renamed))))

	
	


