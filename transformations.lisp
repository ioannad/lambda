;;;;transformations.lisp

(in-package :lambda.transformations)

;;; Transformations between representations of λ terms.
;;
;; So far implemented:
;; - standard->de-bruijn
;; - standard->common-lisp
;;
;; TODO:
;; - de-bruijn->standard
;; - standard->SKI
;; - SKI->standard
;; - standard->mogensen-scott
;; - mogensen-scott->standard
;; - standard->binary
;; - binary->standard
;; - common-lisp->standard (some day?)
;;
;;-------------------------------------------------------------------------


;;; From syntax-standard to syntax-de-bruijn

;; For example,  '(λ x (λ y (z (x y))))
;; should become '(λ   (λ   (z (1 0)))).

(defun standard->de-bruijn (term &optional variable (counter 0))
  "Transformation of λ terms to their de Bruijn representation. This recurses 
both inside the λ abstractions and along a λ term structure, and during this 
double recursion it might be fed either objects of λ term structure or of de
 Bruijn representation structure."
  (cond ((db-abstraction-p term)
	 (list 'λ
	       (standard->de-bruijn (db-abstraction-body term)
				    variable
				    (1+ counter))))
	((numberp term)
	 term)
	(T (^ecase term
		   ((:variable
		     (if (eq term variable)
			 counter
			 term))
		     ((operation operand) :application
		      (list (standard->de-bruijn operation variable counter)
			    (standard->de-bruijn operand   variable counter)))
		     ((var body) :abstraction
		      (list 'λ
			    (standard->de-bruijn
			     ;; here we go one level deeper in the scope of
			     ;; (λ variable ...), if any (variable may be NIL).
			     (standard->de-bruijn body variable (1+ counter))
			     ;; and we start de-bruijning (λ var ...)
			     var))))))))

;; TODO: (defun de-bruijn->standard (term &optional (counter 1))   ( ))

;;; Standard syntax to Common Lisp syntax.

;; To avoid compiler warnings and for a cleaner resulting common lisp expression
;; I use the following helper function to differentiate between those
;; λ abstractions whose body contains the variable of the λ abstraction, and
;; those who don't.

;; For example,
;; - does contain: '(λ x x) should become '(lambda (x) (funcall x)), and when
;;   it's the operation of an application, it should have an argument:
;;   '((λ x x) foo) should become '(funcall (lambda (x) x) foo)
;; - those who don't: '(λ x y) should become '(lambda () (funcall y)), and when
;;   it's the operation of an application, the operand may be discarded:
;;   '((λ x y) foo) should become '(funcall (lambda () y))

(defun real-arg-p (var body)
  (member var (^free-variables body)))

(defun standard->common-lisp (term)
  "Returns a common lisp closure equivalent to the expression."
  (^ecase term
	  ((:variable term)
	   ((operation operand) :application
	    (if (and (^abstraction-p operation)
		     (not (destructuring-bind (var body) (cdr operation)
			    (real-arg-p var body))))
		`(funcall ,(standard->common-lisp operation))
		`(funcall ,(standard->common-lisp operation)
			  ,(standard->common-lisp operand))))
	  ((var body) :abstraction
	   (if (real-arg-p var body)
	       `(lambda (,var) ,(standard->common-lisp body))
	       `(lambda () ,(standard->common-lisp body)))))))

;; TODO (defun common-lisp->standard (expression) )
;; Well. At least some subset of common lisp. ^^

