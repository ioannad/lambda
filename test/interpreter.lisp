;;;;test/interpreter.lisp

(in-package :lambda.test)

(defun test-^eval ()
  (let ((environment (list (assign 'x 'a)
			   (assign 'y 'b)
			   (assign 'r 't))))    
    (assert (equal 'a (^eval 'x environment)))		   
    (assert (equal 'b (^eval '((^ x x) y) environment)))
    (assert (^eq (^eval '((l x (l y (l z (x (y z))))) r) environment)
		 '(^ y (^ z (t (y z))))))))

(defun test-^eval-catch-loop ()
  (let* ((omega (^substitute-environment :omega 
					 (recursing-combinators)))
	 (long-loop-component '(^ x (^ y ((x y) y))))
	 (long-loop-base '((component component) component))
	 (assignment (assign 'component long-loop-component))
	 (long-loop (^substitute long-loop-base assignment)))
    
    (assert (^eq (^eval-catch-loop omega)
		 omega))
    (assert (^eq (^eval-catch-loop long-loop-base
				   (list assignment))
		 long-loop))))

;; Functionality of ^interpret is tested in test/ui.lisp

(defun test-interpreter (&key verbosep)
  (test-^eval)
  (when verbosep
    (format *standard-output* "~&INTEPRETER tests passed.~%")))

