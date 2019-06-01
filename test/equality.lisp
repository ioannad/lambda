;;;;test/equality.lisp

(in-package :lambda.test)

(defun test-equality (&key verbosep)
  (let ((t0  '(^ a (x (a w))))
	(t1  '(^ a (c (a z))))
	(t2  '(^ b (c (b z))))
	(t3  '(^ a (^ a a)))
	(t4  '(^ b (^ a a)))
	(t5  '(^ a (^ a a)))
	(t6  '(^ a (^ c c)))
	(t7  '(^ x (c x)))
	(t8  '(^ x (u x)))
	(t9  '(^ x x))
	(t10 '(^ y y))
	(t11 '(^ a (r (t a))))
	(t12 '(^ b (r (t b))))
	(t13 '((^ g (^ o (foo (o g)))) bar))
	(t14 '((^ g (^ p (foo (p g)))) bar)))
    (assert (mapcar '^eq
		    (list t1 t3 t4 t5 t9  t11 t13)
		    (list t2 t4 t5 t6 t10 t12 t14)))
    (assert (not (^eq t0 t1)))
    (assert (not (^eq t6 t7)))
    (assert (not (^eq t7 t8))))
  (when verbosep
    (format *standard-output* "~&EQUALITY tests passed.~%")))
		 
