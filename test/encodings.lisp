;;;;test/encodings.lisp

(in-package :lambda.test)

(defun test-is-combinator (combinator)
  (assert (null (set-difference (^free-variables combinator)
				(list :Y :omega)))))

(defun test-are-combinators (combinators)
  (mapcar 'test-is-combinator combinators))
  
(defun test-booleans ()
  (assert (eq 'x (^eval '(((:if :true) x) y) (basic-combinators))))
  (assert (eq 'y (^eval '(((:if :false) x) y) (basic-combinators)))))

;; A proof of the fixed point theorem for this implementation of λ calculus.

(defun test-y-combinators ()
    "From [Barendregt 1994; Theorem 2.12(ii) For every =term=,
=(term (:Y term)) = (:Y term)=."
    (let ((terms (list 'a
		       '(a b)
		       '((b a) (λ b (λ c ((a b) (c d))))))))
      (loop for term in terms
	 for YF  = (^eval-step `(:Y ,term) (recursing-combinators))
	 for Y-F = (^eval-step `(:Y- ,term) (recursing-combinators))
	 do (assert (^eq (^eval-step YF)
			 (list term YF)))
	 do (assert (^eq (^eval-step (^eval-step (^eval-step Y-F)))
			 (list term Y-F))))))
			  

;;; Church-numerals

(defun test-church-numerals ()
  (assert (^eq (^eval `((:A+ ,(church-numeral 1)) ,(church-numeral 1))
		      *church-encodings*
		      :use-reduction-strategy #'^reduce-normal)
	       (church-numeral 2)))
  (assert (^eq (^eval `((:A+ ,(church-numeral 2)) ,(church-numeral 7))
		      *church-encodings*
		      :use-reduction-strategy #'^reduce-normal)
	       (church-numeral 9)))
  (assert (^eq (^eval `((:A* ,(church-numeral 2)) ,(church-numeral 7))
		      *church-encodings*
		      :use-reduction-strategy #'^reduce-normal)
	       (church-numeral 14)))
  (assert (^eq (^eval `((:Aexp ,(church-numeral 2)) ,(church-numeral 3))
		      *church-encodings*
		      :use-reduction-strategy #'^reduce-normal)
	       (church-numeral 8))))

;;; Barendregt-numerals

(defun test-barendregt-arithmetic ()
  (let ((encodings (all-encodings))
	(b0 (lambda.encodings::barendregt-numeral 0))
	(b1 (lambda.encodings::barendregt-numeral 1))
	(b2 (lambda.encodings::barendregt-numeral 2))
	(b4 (lambda.encodings::barendregt-numeral 4))
	(b5 (lambda.encodings::barendregt-numeral 5))
	(b7 (lambda.encodings::barendregt-numeral 7)))    
    (assert (^eq (^eval-normal `(:S+ ,b4)   encodings)
		 b5))
    (assert (^eq (^eval-normal `(:P- ,b5)   encodings)
		 b4))
    (assert (^eq (^eval-normal `(:Zero ,b2) encodings)
		 (^eval-normal :false       encodings)))
    (assert (^eq (^eval-normal `(:Zero ,b0) encodings)
		 (^eval-normal :true        encodings)))
    (assert (equal (^eval-normal `(((:if (:Zero ,b0)) a) b)   encodings)
		   'a))
    (assert (equal (^eval-normal `(((:if (:Zero ,b1)) a) b)   encodings)
		   'b))
    (assert (^eq b7 (^eval-normal `((:B+ ,b2) ,b5)            encodings)))
    (assert (^eq b4
		 (^eval-normal `((:B* ,b2) ,b2)               encodings)))))

;;; Barendregt numerals are VERY SLOW.

(defun test-encodings (&key verbosep)
  (test-are-combinators (mapcar (lambda (combi)
				  (^substitute-environment
				   combi
				   (all-encodings)))
				(mapcar 'cdr (all-encodings))))
  (test-booleans)
  (test-y-combinators)
  (test-church-numerals)
  (test-barendregt-arithmetic)
  (when verbosep
    (format *standard-output* "~&ENCODINGS tests passed.~%")))
