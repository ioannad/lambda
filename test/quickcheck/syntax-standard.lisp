;;;;test/quickcheck/syntax-standard.lisp

(in-package :lambda.quickcheck)

;; This file tests the predicates in syntax-standard.lisp, and thus the
;; correctness of the generators in test/quickcheck/generators.lisp

;; - check- prefixes components of a quickcheck- prefixed function.
;; - quickcheck- performs a quickcheck and should be called by
;;   'cl-quickcheck:quickcheck

(defun check-syntax-free-variables (free)
  (is listp free))

(defun quickcheck-predicates (depth verbosep)
  (let ((var-generator          (lambda () (a-^variable)))
	(abstraction-generator  (lambda () (a-^abstraction depth)))
	(babstraction-generator (lambda () (a-binding-^abstraction depth)))
	(application-generator  (lambda () (a-^application depth))))
    (when verbosep
      (print "Generating variables."))
    (for-all (var)
      (is ^variable-p var))
    (when verbosep
      (print "Generating ^abstractions."))
    (for-all (abstraction)
      (is ^abstraction-p abstraction))
    (when verbosep
      (print "Generating abstractions which bind variables in their bodies."))
    (for-all (babstraction)
      (is ^abstraction-p babstraction))
    (when verbosep
      (print "Generating applications."))
    (for-all (application)
      (is ^application-p application))))

(defun check-syntax-closures (terms)
  (loop for term in terms
     do (let ((closure (^closure term)))
	  (is ^term-p closure)
	  (is null (^free-variables closure)))))
  
(defun check-syntax-standard-binding (^symbol var body binding)
  (is ^symbol-p ^symbol)
  (is member var (^free-variables body))
  (isnt member var (^free-variables binding)))

