;;;;syntax-de-bruijn.lisp

(in-package :lambda.syntax-de-bruijn)

;;;; De Bruijn representation of closed λ terms, locally nameless for open
;;;; λ terms.

;;; De Bruijn indices

;; De Bruijn indices make alpha conversion (renaming of bound variables)
;; obsolete. A bound variable becomes a natural number indicating the amount of
;; nested lambdas up to the lambda that binds the variable.

;; For example '(λ x x) is represented as '(λ 0), and
;; '(λ a (λ b (λ c ((a b) c)))) is represented as '(λ (λ (λ ((2 1) 0)))).

;;; De Bruijn syntax
 
;; Name convention:
;; - Call "de Bruijn term" a de Bruijn representation of a lambda term.
;; - The prefix db- is used for functions which expect de Bruijn terms in
;;   their arguments, and for arguments that are supposed to be de Bruijn terms.

;; br-term ::= ^variable | ^application | br-abstraction | 
;; br-abstraction ::= (list ^symbol br-term)

;; With the above BNF, de Bruijn terms are possible, which contain natural
;; numbers larger than the length of nested lambdas containing that number.
;; For example '(λ 2). Such terms are not produced by my transformations though,
;; so not all de Bruijn terms have a standard-syntax representation. (See
;; transformations.lisp.

;; Sometimes de Bruijn representations are defined only for closed terms, that 
;; is λ terms without free variables.

;; For non-closed terms, I keep the free variables as they are. This 
;; representation is sometimes called "locally nameless".

;;----------------------------------------------------------------------------

(defun db-abstraction-p (term)
  "Only checks the superficial structure, so it can be used during 
transformations, in possibly malformed input."
  (and (listp term)
       (^symbol-p (first term))
       (= 2 (length term))))

(defun db-abstraction-body (db-abstraction)
  (second db-abstraction))

(defun db-term-p (db-term)
  "Predicate for well formed de Bruijn representations."
  (or (^variable-p db-term)
      (integerp db-term)
      (and (db-abstraction-p db-term)
	   (db-term-p (second db-term)))
      (and (^application-p db-term)
	   (db-term-p (first db-term))
	   (db-term-p (second db-term)))))      

(defun db-free-variables (db-term)
  (cond ((^variable-p db-term)
	 (list db-term))
	((or (^symbol-p db-term)
	     (numberp db-term))
	 nil)
	(T (reduce 'union
		   (mapcar 'db-free-variables
			   db-term)))))
