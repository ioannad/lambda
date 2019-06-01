;;;;test/quickcheck/reducers.lisp

(in-package :lambda.quickcheck)

(defun check-^reduce-catch-loop (term term% free)
  (let ((var (random-element free)))
    (is ^term-p
	(^reduce-catch-loop
	 `((λ  ,var term) ,term%)))))

(defun check-reducers (term term% free)
  (check-^reduce-catch-loop term term% free))

 
