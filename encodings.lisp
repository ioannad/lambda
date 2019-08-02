;;;;encodings.lisp

;;; Some standard encodings for this λ calculus.

;; Encodings are assignments of a keyword name to a combinator λ term, that is
;; a lambda term without free variables. Therefore these codes are always 
;; determined by their de Bruijn representation.

;; From Barendregt's "Introduction to λ calculus"

(in-package :lambda.encodings)

;; The basic definitions	      

(defun basic-combinators ()   ; => list of assignments
  "Standard combinators"                ; Barendregt names
  (list (assign :id    '(λ a a))        ; I
	(assign :true  '(λ a (λ b a)))  ; K
	(assign :false '(λ a (λ b b)))  ; K*
	(assign :S     '(λ a (λ b (λ c ((a c) (b c)))))) 
	(assign :and   '(λ a (λ b ((a b) a))))
	(assign :or    '(λ a (λ b ((a a) b))))
	(assign :if    '(λ a (λ b (λ c ((a b) c)))))))

;; The Barendregt names in the inline comments above are the ones used in the
;; SKI-Calculus. 

(defun recursing-combinators ()
  (list (assign :Y     '(λ a ((λ b (a (b b))) (λ c (a (c c))))))
	(assign :Y-    '((λ z (z z)) (λ z (λ f (f ((z z) f))))))
	(assign :Y--   '((λ x (λ y ((x y) x))) (λ y (λ x (y ((x y) x))))))
	(assign :omega '((λ a (a a)) (λ a (a a))))))

;;; Operations returning encodings from terms

(defun ^boolean-p (term) ; note this is unlike the T - NIL convention in lisp
  (or (^eq term '(λ a (λ b a))) ; true
      (^eq term '(λ a (λ b b))))) ; false

(defun ^pair (term-1 term-2)
  "Returns an encoded pair of TERM-1 and TERM-2."
  (let ((gensym (gensym)))
    `(λ ,gensym ((,gensym ,term-1) ,term-2))))

(defun ^first (pair-term)
  `(,pair-term :true))

(defun ^second (pair-term)
  `(,pair-term :false))

;;; Church numerals and their arithmetic

(defun apply-n-times (operation operand n)
  (if (= n 0)
      operand
      (list operation (apply-n-times operation operand (1- n)))))

(defun church-numeral (n) ; => λ expression
  (assert (>= n 0))
  (let ((body (apply-n-times 'f 'x n)))
    `(λ f (λ x ,body))))

(defun church-arithmetic () ; => list of assignments
  (list (assign :A+   '(λ a (λ b (λ c (λ d ((a c) ((b c) d)))))))
	(assign :A*   '(λ a (λ b (λ c (a (b c))))))
	(assign :Aexp '(λ a (λ b (b a)))) ; arithmetic exponentiation
	(assign :Azero? '(λ n ((n (λ a :false)) :true))) ; test if n is zero
	(assign :A+1 '(λ b (λ c (λ d (c ((b c) d))))))
	(assign :A-1 '(λ b (λ c (λ d (((b (λ e (λ f (f (e c)))))
				       (λ g d))
				      :id)))))
	(assign :A-factorial `(:Y (λ c (λ n (((:if (:Azero? n))
					      ,(church-numeral 1))
					     ((:A* (c (:A-1 n)))
					      n))))))))

;;; Barendregt numerals and their arithmetic

(defun barendregt-numeral (n)
  (assert (>= n 0))
  (^substitute-environment
   (if (= 0 n)
       :id
       (^pair :false (barendregt-numeral (1- n))))
   (basic-combinators)))
	     
(defun barendregt-arithmetic () ; => list of assignments
  (list (assign :S+   `(λ a ,(^pair :false 'a)))
	(assign :P-   `(λ a (a :false))) 
	(assign :Zero '(λ a (a :true)))
	(assign :B+   '(:Y (λ c (λ x (λ y (((:if (:Zero x))
					    y)
					   (:S+
					    ((c (:P- x))  y))))))))
	(assign :B* `(:Y (λ c (λ x (λ y (((:if (:Zero x))
					  ,(barendregt-numeral 0))
					 ((:B+ ((c (:P- x)) y))
					  y)))))))
	(assign :B-factorial `(:Y (λ c (λ n (((:if (:Zero n))
					      ,(barendregt-numeral 1))
					     ((:B* n) (c (:P- n))))))))))

;; The following is creating syntactic sugar, for use with
;; lambda.repl:repl-church.
;; It also also serves as a validator for that REPL.

(defun encode-church-term (pre-term) ; => ^term
  "Takes an expression such as '((:A+ 2) 3) and returns
`((:A+ ,(church-numeral 2)) ,(church-numeral 3))."
  (if (numberp pre-term)
      (church-numeral pre-term)
      (do-abstractions pre-term
	(lambda (var body)
	  `(λ ,var ,(encode-church-term body)))
	#'encode-church-term)))
	  

