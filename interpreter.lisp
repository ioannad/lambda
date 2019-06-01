;;;;interpreter.lisp

(in-package :lambda.interpreter)

;; Evaluating here means reducing in an environment. Lambda calculus does not
;; have environments per se, so here an environment is a list of assignments of
;; variables to λ terms.

;; For a non-reducing version use lambda.substitution:^substitute-environment.

(defun ^eval (term environment
	      &key (use-reduction-strategy '^reduce))
  (funcall use-reduction-strategy
	   (^substitute-environment term environment)))

(defun ^eval-normal (term environment)
  (^eval term environment :use-reduction-strategy '^reduce-normal))

;;; Catching some infinite loops.

;; Experiments with a notion of equality for the eventually self-evaluating
;; functions (loops longer than 1 step possible).
;;
;; There are several possibilities here, variating on the substitution steps
;; and/or reduction steps. Below I explore fully substituting and using
;; ^reduce-step and ^reduce-step-catch-loop.

(defun ^eval-step (term &optional environment)
  "When the environement is NIL, this is just ^reduce-step."
  (^reduce-step (^substitute-environment term environment)))

(defun ^eval-normal-steps (n term environment
			   &aux (temp (^substitute-environment
				       term environment)))
  (loop for i from 1 to n
     do (setf temp (^reduce-normal-step temp)))
  temp)
	       
(defun ^eval-catch-loop (term environment  ; => ^term
			 &key (max-steps NIL))
  "Lazy step"
  (^reduce-catch-loop (^substitute-environment term environment)
		      :max-steps max-steps))

(defun ^eval-normal-catch-loop (term environment ; => ^term
				&key (max-steps NIL)) 
  (^reduce-normal-catch-loop (^substitute-environment term environment)
			     :max-steps max-steps))

;;; The interpreters for :lambda.repl

;; Creating the necessary environments

(defun simplify-in-assignment (assignment  ; => assignment
			       &optional (environment (basic-combinators)))
  (destructuring-bind (name . encoding) assignment
    (assign name (^eval-normal-catch-loop encoding environment))))

(defun simplify-in-assignments (assignments ; => environment
				&optional (environment (basic-combinators)))
  (loop for assignment in assignments
     collect (simplify-in-assignment assignment environment)))

(defun basic-encodings () ; => environment 
  (reverse (simplify-in-assignments (basic-combinators))))

(defun base-encodings () ; => environment 
  (append (basic-encodings)
	  (recursing-combinators)))

(defun church-encodings () ; => environment
  (append (reverse
	   (simplify-in-assignments (church-arithmetic)
				    (append (church-arithmetic)
					    (basic-encodings))))
	  (base-encodings)))
	  
(defun all-encodings () ; => environment 
  (append (reverse 
	   (simplify-in-assignments
	    (lambda.encodings::barendregt-arithmetic)))
	  (church-encodings)))

;; An interpreter for encoded terms

(defun ^interpret (term environment
		   &key
		     (reduction #'^reduce-normal)
		     (decoding #'decode-encodings-in-term))
  "The eval-print part of a repl."
  (let ((answer (^eval term environment
		       :use-reduction-strategy reduction)))
    (funcall decoding answer environment)))



