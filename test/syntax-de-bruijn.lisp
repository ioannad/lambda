;;;;test/syntax-de-bruijn.lisp

(in-package :lambda.test)

(defun test-syntax-de-bruijn (&key verbosep)
  (let ((db-term-1 '(^ (^ (x (1 (y 0))))))
	(db-term-2 '(a (b (^ (c (^ 1)))))))
    (assert (db-abstraction-p db-term-1))
    (assert (equal '(x (1 (y 0)))
		   (db-abstraction-body
		    (db-abstraction-body db-term-1))))
    (assert (set-equal (db-free-variables db-term-1)
		       '(x y)))
    (assert (set-equal (db-free-variables db-term-2)
		       '(a b c)))
    (assert (every 'db-term-p
		   (list db-term-1 db-term-2))))
  (when verbosep
    (format *standard-output* "~&SYNTAX-DE-BRUIJN tests passed.~%")))
