;;;;test/reducers.lisp

(in-package :lambda.test)

(defun test-reducer-strategies (term reduct)
  (assert (^eq (^reduce term :use-strategy-stepper '^reduce-lazy-step)
	       reduct))
  (assert (^eq (^reduce term :use-strategy-stepper '^reduce-normal-step)
	       reduct))
  (assert (^eq (^reduce term :use-strategy-stepper '^reduce-applicative-step)
	       reduct)))

(defun test-reducing-loop ()
  (let ((omega  '((^ x (x x)) (^ x (x x)))))
    (assert (^eq (lambda.reducers::^reduce-normal-step omega)
		 omega))
    (assert (^eq (^reduce-catch-loop omega)
		 omega))))

(defun test-reducers (&key verbosep)
  (let* ((term-1   'x)
	 (reduct-1 'x)
	 (term-2   '((λ x x) y))
	 (reduct-2 'y)
	 (term-3   '(((^ a (^ b (a b))) x) y))
	 (reduct-3 '(x y))
	 (term-4   '((l x (l y (l z (x (y z))))) a))
	 (reduct-4 '(^ y (^ z (a (y z)))))
	 (term-5   '((^ a (^ b b)) ((^ x (x x)) (^ x (x x)))))
	 (reduct-5 '(λ b b))
	 (term-6   '((^ a (c a)) (λ a (d a))))
	 (reduct-6 '(c (λ a (d a)))))
    (mapcar 'test-reducer-strategies
	    (list term-1   term-2   term-3   term-4   term-5   term-6)
	    (list reduct-1 reduct-2 reduct-3 reduct-4 reduct-5 reduct-6)))

  (test-reducing-loop)
  (when verbosep
    (format *standard-output* "~&REDUCERS tests passed.~%")))
