;;;;test/compiler.lisp

(in-package :lambda.test)

;; Compiler tests

(defun simple-compiler-tests ()
  (assert (eq 5 (funcall (^compile 'x) 5)))
  (assert (eq 4 (funcall (^compile '(^ x x)) 4)))
  (assert (equal (^closure '(x y))
		 '(λ y (λ x (x y)))))
  (assert (eq 5 (funcall (funcall (^compile '(x y)) 5 ) (lambda (c) c))))
  (assert (eq 5 (funcall (^compile `((,(^closure '(x y)) N) (^ c c)))
			 5)))
  ;; Every λ term ^compiles to a Common Lisp function.
  (assert (equal (type-of (^compile 'x)) 'FUNCTION))
  (assert (equal (type-of (^compile '((λ x x) (λ x x)))) 'FUNCTION))
  (assert (equal (type-of (^compile '(λ x y))) 'FUNCTION)))

(defun test-compiler (&key verbosep)
  (simple-compiler-tests)
  (when verbosep
    (format *standard-output* "~&COMPILER tests passed.~%")))
