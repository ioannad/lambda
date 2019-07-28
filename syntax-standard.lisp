;;;;syntax-standard.lisp

(in-package :lambda.syntax-standard)

;;; Standard syntax of lambda calculus
;;
;;
;; The parameter ^term stands for "lambda term", a term in the following
;; "standard syntax". Similarly for ^variable, ^application, ^abstraction, and
;; ^symbol.
;;
;; ^term ::= ^variable | ^application | ^abstraction
;; ^variable ::= any common-lisp symbol except '^, 'λ, 'l
;; ^application ::= (list ^term-1 ^term-2)
;; ^abstraction ::= (list ^symbol ^variable ^term)
;; ^symbol ::= '^ | 'λ | 'l
;;
;; The symbol ^ may be used for "lambda" because it resembles a capital
;; λ (Λ). The character l as well, for when λ or ^ are not available. 
;;
;;----------------------------------------------------------------------------


;;;; Syntax predicates and conditional
;;
;; The first predicates are superficial, in that they don't check if the
;; subterms are indeed λ-terms. This makes them useful
;; in transformation steps, in which a recursion is building a different
;; representation of a λ term, and the subterms might be malformed.
;; Having superficial predicates, gives a flexible and extensible
;; exhaustive conditional (^ecase below), for branching on the structure of a
;; λ term.
;;
;; Name convention:
;; Functions that take ^terms as some of their arguments are prefixed with ^,
;; especially when without the ^, the function name is a common-lisp function
;; name.

;;; Superficial predicates

(defun ^symbol-p (symbol)
  (member symbol '(^ λ l)))

(defun ^variable-p (symbol)
  (and (symbolp symbol)
       (not (^symbol-p symbol))))

(defun ^abstraction-p (term)
  (and (listp term)
       (eq 3 (length term))
       (^symbol-p (first term))))

(defun ^application-p (term)
  (and (listp term)
       (eq 2 (length term))))

;;; Error throwing exhaustive case conditional for λ terms.

(defmacro ^ecase (term ((&key variable)
			((operation operand) &key application)
			((var body) &key abstraction)))
  "This is an exhaustive conditional for branching on the structure of a λ 
term."
  `(cond ((^variable-p ,term)
	  ,variable)
	 
	 ((^application-p ,term)
	  (let ((,operation (first ,term))
		(,operand   (second ,term)))
	    ,application))
	 
	 ((^abstraction-p ,term)
	  (let ((,var (second ,term))
		(,body (third ,term)))
	    ,abstraction))
	 
	 (T (error "~%Not the structure of a λ term: ~a~%" ,term))))

;;; Error throwing recursing ^term predicate.

(defun ^term-p (term)
  "Checks in the entire structure of a term recursively."
  (^ecase term
	  ((:variable term)
	   ((operation operand) :application
	    (when (and (^term-p operation)
		       (^term-p operand))
	      term))
	   ((var body) :abstraction
	    (when (and (^variable-p var)
		       (^term-p body))
	      term)))))

;;; Recursing on the structure of a ^term.

;; do recursing on the structure of a λ term
    
(defun do-applications (term applications-action starting-function
				 &rest args)
  "APPLICATIONS-ACTION must be a function of two arguments, operation and 
operand. STARTING-FUNCTION should be a function of at least one argument, 
a λ term. Any more arguments that STARTING-FUNCTION could need, may be passed
as rest ARGS."
  (^ecase term
	  ((:variable term)
	   ((operation operand) :application
	    (funcall applications-action operation operand))
	   ((var body) :abstraction
	    (list 'λ var (apply starting-function
				(cons body args)))))))

(defun do-abstractions (term abstractions-action starting-function
				 &rest args)
  "ABSTRACTIONS-ACTION should be a function of two arguments, var and body.
STARTING-FUNCTION should be a function of at least one argument, a λ term. Any 
more arguments that STARTING-FUNCTION could need, may be passed as rest ARGS."
  (^ecase term
	  ((:variable term)
	   ((operation operand) :application
	    (list (apply starting-function
			 (cons operation args))
		  (apply starting-function
			 (cons operand args))))
	   ((var body) :abstraction
	    (funcall abstractions-action var body)))))

;;;; Operations on lambda terms

;;; Free and bound variables of a lambda term

;; Definition: Free variables are ones not bound by an abstraction.

(defun ^free-variables (term)
  (remove-duplicates
   (^ecase term
	   ((:variable
	     (list term))
	    ((operation operand)
	     :application
	     (append (^free-variables operation)
		     (^free-variables operand)))
	    ((var body)
	     :abstraction
	     (set-difference (^free-variables body)
			     (list var)))))))

(defun ^free-variable-p (variable term)
  (member variable (^free-variables term)))

(defun ^variables (term)  ; => (list free-variables bound-variables)
  "The bound variables are being concatenated, for checks whether a bound 
variable is bound in more than one subterms of term."
  (^ecase term
	  ((:variable
	    (list (list term) nil))
	   ((operation operand) :application
	    (destructuring-bind ((free-operation bound-operation)
				 (free-operand   bound-operand))
		(list (^variables operation)
		      (^variables operand))
	      (list (remove-duplicates
		     (append free-operation free-operand))
		    (append bound-operation bound-operand))))
	   ((var body) :abstraction
	    (destructuring-bind (free-body bound-body)
		(^variables body)
	      (list (set-difference free-body
				    (list var))
		    (append (list var)
			    bound-body)))))))

(defun ^bound-variables (term)
  (remove-duplicates (second (^variables term))))

;;; Syntactic operations on λ terms

(defun ^abstract (term variable)
  "Returns an abstraction binding the VARIABLE in the body TERM."
  (list 'λ variable term))

;; According to [Barendregt 1984, Definition 2.1.7(v)], a closure of a λ term M
;; is an abstraction of all free variables in M. For example, the closure of
;; '(x y) is '(λ x (λ y (x y))).

(defun ^closure (term &key
			(free-variables (^free-variables term)))
  (reduce '^abstract free-variables
	  :initial-value term))



