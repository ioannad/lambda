;;;;equality.lisp

(in-package :lambda.equality)

;; When other, more compact representations, such as the binary, or SKI-calculus
;; get defined, more equalities based on those representations will be added
;; here.


;;; Equality via de Bruijn representations

(defun ^eq (term-1 term-2) ; => T or NIL
  "These two terms have the same de Bruijn representation, and the same
free variables in the same places (if any). They are identical λ terms."
  (equal (standard->de-bruijn
	  term-1)
	 (standard->de-bruijn
	  term-2)))

