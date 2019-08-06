;;;;test/all.lisp

(in-package :lambda.test)

(defun test-all (&key (verbosep T))
  "Runs all the manual (non-quickcheck) tests."
  (let ((terms '((λ y (λ x ((foo (λ r bar)) baz)))
		 (λ y (λ x (x ((a (y z)) (y v)))))
		 ((l t t) z)
		 (λ a (λ b (c (λ d f))))
		 (λ x x)
		 (λ x (x u)))))
    
    (format *standard-output* "~&---------------------------~%")
    (format *standard-output* "~&Running standard tests.~%")
    (format *standard-output* "~&---------------------------~%")
    
    (test-syntax-standard terms :verbosep verbosep)
    (test-syntax-de-bruijn :verbosep verbosep)
    (test-syntax-ski :verbosep verbosep)
    (test-transformations terms :verbosep verbosep)
    (test-renames :verbosep verbosep)
    (test-equality :verbosep verbosep)
    (test-substitution :verbosep verbosep)
    (test-reducers :verbosep verbosep)
    (test-encodings :verbosep verbosep)
    (test-decodings :verbosep verbosep)
    (test-interpreter :verbosep verbosep)
    (test-ui terms :verbosep verbosep)
    (test-compiler :verbosep verbosep)))
