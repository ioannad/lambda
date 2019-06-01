;;;;test/substitution.lisp

(in-package :lambda.test)

(defun test-^substitute ()
  (assert (^eq (^substitute
		'(^ x (x y))
		(assign 'y 3))
	       '(^ X (X 3))))

  (assert (^eq (^substitute '((a b) (c a))
			    '(a . (b a)))
	       '(((B A) B) (C (B A)))))
  
  (destructuring-bind (l1 a (g (l2 b (a% (b% c)))))
      (^substitute   '(^  a (b (^  b (a  (b  c)))))
		     (assign 'b 'g))
    ;; (^ A (G (^ B (A (B C)))))
    (assert (every '^symbol-p (list l1 l2)))
    (assert (and (eq a  a%)
		 (eq b  b%)
		 (eq g 'g)
		 (eq c 'c)))))

(defun test-^substitute-environment ()
  (assert (^eq (^substitute-environment
		'((a (^ b ((^ c (d c)) (r f)))) (f b))
		'((a . a%) (b . b%) (f . f%) (c . c%)))
	       '((a% (^ b ((^ c (d c)) (r f%)))) (f% b%)))))

(defun test-substitution (&key verbosep)
  (test-^substitute)
  (test-^substitute-environment)
  (when verbosep
    (format *standard-output* "~&SUBSTITUTION tests passed.~%")))
