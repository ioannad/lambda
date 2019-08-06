;;;;syntax-ski.lisp

(in-package :lambda.syntax-ski)

;;; Combinatorial Logic a.k.a. SKI-calculus
;;; and binary combinatorial logic

;; Here I define some basic functions for transforming ski-calculus terms to
;; and from λ calculus, and ski-calculus terms to and from binary ski-calculus.

;; I use the SKI-calculus presentation of [Section 3; Tro18]:

;; "
;;   Combinatory Logic (CL) is the equational theory of combinators—terms built
;;   up, using application only, from the two constants K and S,
;;   [...]
;;   I is considered a shorthand for S K K.
;; "
;; 
;; I shall denote the combinators by :S, :K, and :I. 
;;
;; SKI-calculus can be seen as a subset of λ calculus, which contains the
;; λ terms which do not contain any ^abstractions. Therefore the syntax-standard
;; predicates ^variable, ^application-p can also be used with SKI-calculus
;; terms.

;; Note that ^variable has a different meaning in SKI-calculus as it does in
;; λ calculus, because :SKI-calculus has two (three) primitives: :S,:K, and
;; in some cases :I. I will not need a more specialised SKI-calculus predicate.

;;; other SKI-calculus predicates

(defun ski-term-p (term)
  (or (^variable-p term)
      (and (^application-p term)
	   (ski-term-p (first term))
	   (ski-term-p (second term)))))

;; It seems more efficient to define a new function 'ski-free-variables than to
;; reuse ^free-variables from :lambda.syntax-standard.

(defun ski-free-variables (ski-term)
  "Free variables in SKI-calculus are ^variables which are not :S, :K, or :I."
  (assert (ski-term-p ski-term))
  (labels ((collector (ski-term)
	     (if (^variable-p ski-term)
		 (list ski-term)
		 (append (collector (first ski-term))
			 (collector (second ski-term))))))
    (remove-duplicates
     (remove-if (lambda (var) (member var '(:S :K :I)))
		(collector ski-term)))))

(defun ski-combinator-p (ski-term)
  "Includes a (SKI-TERM-P SKI-TERM) check in SKI-FREE-VARIABLES."
  (null (ski-free-variables ski-term)))
