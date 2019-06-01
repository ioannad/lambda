;;;;reducers.lisp

(in-package :lambda.reducers)

;;; β Reducers and order of evaluation

;; The principal axiom scheme of λ-calculus, according to [page ;Barendregt 1994], is
;; the following,
;;             ((λ x M) N) = M[x:=N]
;; where M[x:=N] is (^substitute M (assign x N)).

;; Definition. A term of the form ((λ x M) N) is called a β-redex.

(defun β-redex-p (term)
  (and (^application-p term)
       (^abstraction-p (first term))))

;; To β-redexes, the principal axiom scheme can be applied, and result in what
;; is called a reduction of the β-redex. 

(defun reduce-β-redex (β-redex)
  (destructuring-bind ((lamda var body) operand)
      β-redex
    (declare (ignorable lamda))
    (destructuring-bind (new-var new-body)
	(cdr (α-convert var body))
      (^substitute new-body (assign new-var operand)))))

;; Definition. A term is in normal form when it does not include any β-redexes
;; in its subterms.

(defun ^normal-form-p (term)
  (and (not (β-redex-p term))
       (^ecase term
	       ((:variable T)
		((operation operand) :application
		 (and (^normal-form-p operation)
		      (^normal-form-p operand)))
		((var body) :abstraction
		 (^normal-form-p body))))))

;; Not all λ terms have a normal form. For example, if a β-redex returns itself,
;; the reduction has to be endlessly repeated. 

;;; Order of evaluation

;; Many subterms of a term may be β-redexes, which can be found and be β-reduced
;; by looking inside abstractions (no branching), or in each "branch"
;; of an application. A reduction strategy, decides the order of evaluation,
;; that is, the order in which β-redex subterms are reduced, in the case of an
;; application "branching". Below are definitions for different orders of evaluation.

;; Definition. The normal order of reduction first reduces the leftmost β-redex 
;; in each step, that is it reduces the operation part of an application first,
;; and if the operation is a variable, then it reduces the operand.

;; It is a theorem of lambda calculus, that with this reduction strategy, if a
;; term has a β-normal form, then this normal form will be reached.

;; Definition. The applicative order of evaluation reduces the operand part of
;; an application first.

;; This is more efficient, when the operation uses the operand more than once,
;; as in '((λ a (a a)) ((λ b (c b)) d)). Following the normal order, the
;; subexpression '((λ b (c b)) d) will have to be reduced twice to '(c d),
;; whereas the applicative order will reduce this subexpression first, and then
;; reduce the resulting '((λ a (a a)) (c d)) to '((c d) (c d)).

;; A reducer is called lazy, when it never evaluates the operands of a
;; λ application.

;; Since the decisive part of the evaluation is what happens at subterms which
;; are applications, we can define the following general form of a reducer
;; stepper.

(defun ^reduce-strategy-step (term strategy)
  "STRATEGY must be a function of two arguments: operation operand. It decides
which evaluation path to choose in the case of an application."
  (if (β-redex-p term)
      (reduce-β-redex term)
      (do-applications term
	strategy
	#'^reduce-strategy-step strategy)))

;; My first intention is to try and catch loops, so I can run tests. I start
;; with a lazy normal-order stepper, which reduces the current top or left
;; β-redex, that is, decending into abstractions and into the operations of
;; applications which are not β-redexes, and stopping at variables. 

(defun ^reduce-lazy-strategy (operation operand)
  (list (^reduce-lazy-step operation)
	operand))

(defun ^reduce-lazy-step (term)
  (^reduce-strategy-step term #'^reduce-lazy-strategy))
  
;; To reduce numerals to their standard form, we need to also reduce into
;; operands, when the operation is a ^variable, so there I will need the
;; standard normal-order reduction.

(defun ^reduce-normal-strategy (operation operand)
  (let ((left-next (^reduce-normal-step operation)))
    (if (^eq left-next operation)
	(list operation (^reduce-normal-step operand))
	(list left-next operand))))

(defun ^reduce-normal-step (term)
  (^reduce-strategy-step term '^reduce-normal-strategy))

;; To reduce numerals to their standard form, applicative order can also
;; be used.

(defun ^reduce-applicative-strategy (operation operand)
  (let ((right-next (^reduce-applicative-step operand)))
    (if (^eq right-next operand)
	(list (^reduce-applicative-step operation) operand)
	(list operation right-next))))

(defun ^reduce-applicative-step (term)
  (^reduce-strategy-step term '^reduce-applicative-strategy))

;; "Arbitrary" length loop catch reducer, with optional max-steps, and
;; configurable strategy stepper.

(defun ^reduce-catch-loop (term
			   &key
			     (use-strategy-stepper '^reduce-lazy-step)
			     (previous-terms (list term))
			     (max-steps NIL))
  (let ((next-term (funcall use-strategy-stepper term)))
    (if (or (and max-steps
		 (<= max-steps (length previous-terms)))
	    (member next-term previous-terms :test '^eq))
	next-term
	(^reduce-catch-loop next-term
			    :use-strategy-stepper use-strategy-stepper
			    :previous-terms (append (list next-term)
						    previous-terms)
			    :max-steps max-steps))))

;; Using ^eq in each reducer step is costly.

;; TODO: - implement de Bruijn represantation based reduction.
;;       - implement Mogensen's fast reduction

;; The following are some defaults, for the lazy evaluation which I
;; heavily use in :lambda.quickcheck

;; TODO: Consider removing all these defaults.

(defun ^reduce (term &key (use-strategy-stepper #'^reduce-lazy-step))
  (let ((reduced (funcall use-strategy-stepper term)))
    (if (or (equal term reduced)
	    (^eq term reduced))
	reduced
	(^reduce reduced :use-strategy-stepper use-strategy-stepper))))

(defun ^reduce-step (term)
  (^reduce-lazy-step term))

(defun ^reduce-normal (term)
  (^reduce term :use-strategy-stepper #'^reduce-normal-step))

;; A default for testing.

(defun ^reduce-normal-catch-loop (term &key (max-steps NIL))
  (^reduce-catch-loop term
		      :use-strategy-stepper '^reduce-normal-step
		      :max-steps max-steps))
