;;;;substitution.lisp

(in-package :lambda.substitution)

;;; Semantics (values)

;; Substitution is the process of assigning values to variables.
;;
;; The value of a λ-term is a λ-term.
;;
;;----------------------------------------------------------------------------


;;; Assignments

;; I substitute free variables of a λ term using assignments. An assignment is a
;; cons of a variable and a value.

(defun assign (variable value)
  (cons variable value))

(defun assigns-p (variable assignment)
  (equal variable (car assignment)))

(defun conflicts-p (variable assignment)
  (equal variable (cdr assignment)))

;;; Substitution

(defun ^substitute-variable (variable assignment)
  (if (assigns-p variable assignment)
      (cdr assignment)
      variable))
-
(defun ^substitute-application (operation operand assignment)
  (list (^substitute operation assignment)
	(^substitute operand assignment)))

(defun ^substitute-abstraction (var body assignment)
  (cond
    ((assigns-p var assignment) (list '^ var body)) ; assignment gets shadowed
    ((conflicts-p var assignment) (let* ((α-converted (α-convert var body))
					 (var% (second α-converted))
					 (body% (third α-converted)))
				    (^substitute-abstraction 
				     var% body% assignment)))
    (T (list '^ var (^substitute body assignment))))) ; continue substituting

(defun ^substitute (term assignment)
  (^ecase term
	  ((:variable
	    (^substitute-variable term assignment))
	   ((operation operand) :application
	    (^substitute-application operation operand assignment))
	   ((var body) :abstraction
	    (let* ((α-converted (α-convert var body))
		   (var% (second α-converted))
		   (body% (third α-converted)))
	      `(λ ,var% ,(^substitute body% assignment))))))) 

;; Syntactically, λ calculus does not have environments, but occasionally we
;; want to assign (substitute) many variables in one term. I implement
;; environments for this purpose as lists of assignments.

(defun ^substitute-environment (term environment)
  "An environment is a list of assignments. A different substitution may be used
during evaluation (see for example interpreter.lisp)."
  (reduce #'^substitute environment :initial-value term))

;;; Unsafe substitutions towards a prettify-term function, to be used in the UI.

(defun substitute-unsafe-^abstraction (var body assignment)
  (destructuring-bind (old-var . new-var) assignment
    (if (equal var old-var) 
	(list 'λ new-var (substitute-unsafe body assignment))
	(list 'λ var (substitute-unsafe body assignment)))))

(defun substitute-unsafe (term assignment)
  (if (stringp term)
      term
      (^ecase term
	      ((:variable 
		(^substitute-variable term assignment))
	       ((operation operand) :application
		(list (substitute-unsafe operation assignment)
		      (substitute-unsafe operand   assignment)))
	       ((var body) :abstraction
		(substitute-unsafe-^abstraction var body assignment))))))

(defun full-α-conversion (term)
  "Safety measure for prettify-term and for the compiler."
  (do-abstractions term
    (lambda (var body)
      (destructuring-bind (new-var new-body)
	  (cdr (α-convert var body))
	(list 'λ new-var (full-α-conversion new-body))))
    'full-α-conversion))

(defun substitute-unsafe-zip (term variables names
			      &aux (term% term))
  (loop
     for variable in variables
     for name in names
     do (setf term% (substitute-unsafe term%
				       (assign variable name))))
  term%)


;; The following is a horrible ugly function. See TODO list for plans of
;; eliminating these ugly UI-needed printing functions here and in the UI.

(defun prettify-term (term bound-variable-names)
  "Renames the bound variables of term with names in bound-variable-names,
and formats keyword free variables for better printing. This does NOT return a
λ term, the keywords are turned to strings."
  (destructuring-bind (free bound)
      (^variables term)
    (let* ((bound-to-change (reverse
			     (set-difference bound
					     bound-variable-names)))
	   (bound-names-to-use (subseq (reverse
					(set-difference bound-variable-names
							bound))
				       0 (length bound-to-change)))
	   (keywords (remove-if-not 'keywordp free)))
      (cond ((or (intersection bound free)
		 (< (length (^bound-variables term))
		    (length bound)))
	     (prettify-term (full-α-conversion term)
			    bound-variable-names))
	    
	    (T (substitute-unsafe-zip
		(substitute-unsafe-zip term bound-to-change bound-names-to-use)
		keywords
		(mapcar (lambda (keyword) (format nil "~S" keyword)) keywords)))))))
