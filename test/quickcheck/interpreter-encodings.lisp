;;;;test/quickcheck/interpreter-encodings.lisp

(in-package :lambda.quickcheck)

;; The names below refer to Barendregt's 1984 book "The Lambda Calculus, its
;; Syntax and Semantics".
;;

(defun check-barendregt-example-2-10 (term term% term%%)
  "From [Barendregt 1994; Example 2.10]."
  (is ^eq
      (^eval-step `(:id ,term) (basic-combinators))
      term)
  (is ^eq
      (^eval-step
       (^eval-step `((:true ,term) ,term%) (basic-combinators)))
      term)
  (is ^eq
      (^eval-step
       (^eval-step `((:false ,term) ,term%) (basic-combinators)))
      term%)
  (is ^eq
      (^eval-step
       (^eval-step
	(^eval-step
	 `(((:S ,term) ,term%) ,term%%) (basic-combinators))))
      `((,term ,term%%) (,term% ,term%%))))

(defun check-fixed-point-theorem-i (term)
  "From [Barendregt 1994; Theorem 2.12(i) For every =term=, there is a term
=W = (λ x (term (x x)))= and a term =X = (W W)=, such that =(term X) = X=."
  (let ((x (gensym)))
    (flet ((W (term%) `(λ ,x (,term% (,x ,x)))))
      (let ((X (list (W term)
		     (W term))))
	(is ^eq
	    (list term X)
	    (lambda.reducers::reduce-β-redex X))))))

(defun check-fixed-point-theorem-ii (term)
  "From [Barendregt 1994; Theorem 2.12(ii) For every =term=,
=(term (:Y term)) = (:Y term)=."
  (let ((YF (^eval-step `(:Y ,term) (recursing-combinators))))
    (is ^eq
	(^eval-step YF)
	(list term YF))))

(defun barendregt-example-2.13 (term)
  "From [Barendregt 1994; Example 2.13]: 
=(latex)\exists G \forall X, (G X) = ((:S G) X)=."

  ;; TODO
  )
    
(defun check-interpreter-encodings (term term% term%%)
  (check-barendregt-example-2-10 term term% term%%)
  (check-fixed-point-theorem-i term)
  (check-fixed-point-theorem-ii term))
  
  
      
 
