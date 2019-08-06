;;;;test/syntax-ski.lisp

(in-package :lambda.test)

(defun test-ski-term-p ()
  (assert (ski-term-p '((:S x) ((:K :K) :I))))
  (assert (ski-term-p ':S))
  (assert (not (ski-term-p '())))
  (assert (not (ski-term-p '(a b c)))))

(defun test-ski-free-variables ()
  (flet ((assert-has-free (ski-term free)
	   (assert (set-equal (ski-free-variables ski-term)
			      free))))
    (assert-has-free '(((:S x) :K) (:I (y x))) '(x y))
    (assert-has-free '((((((:S :I) :K) :S) :K) :I) x) '(x))
    (assert-has-free :S nil)
    (assert-has-free 'x '(x)))
  (handler-case (ski-free-variables '())
    (error (error)
      (declare (ignore error)))
    (:no-error ()
      (error "SKI-FREE-VARIABLES not throwing error for empty list argument."))))


(defun test-ski-combinator-p ()
  (assert (ski-combinator-p
	   '(((:S :S) :I) (:K (:I :S)))))
  (assert (ski-combinator-p
	   :I)))

(defun test-syntax-ski (&key verbosep)
  (test-ski-term-p)
  (test-ski-free-variables)
  (test-ski-combinator-p)

  (when verbosep
    (format *standard-output* "~&SYNTAX-SKI tests passed.~%")))

