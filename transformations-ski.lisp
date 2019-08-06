;;;;transformations-ski.lisp

(in-package :lambda.transformations)

;;; Tromp's compact bracket abstraction "λ²" from [Section 3.2; Tro18]

;; I define each rule separately and add documentation from Tromp's paper.
;; Note that Tromp is using left associativity of application. I do not use
;; this syntactic sugar, to keep my functions simpler. In the documentation
;; strings, I added the "missing" parens.

;; I use the variable SKI where a ski-term should go.

(defun λ²-rule-1 (var ski)
  "λ² x. ((S K) M) ≡ (S K), (for all M)
Exploits the fact that S K M behaves as identity."
  (when (and (listp ski)
	     (equal '(:S :K)
		    (first ski)))
    '(:S :K)))

(defun λ²-rule-2 (var ski)
  "λ² x. M ≡ K M, (x not in M)
Standard bracket abstraction rule."
  (when (not (member var (ski-free-variables ski)))
    `(:K ,ski)))

(defun λ²-rule-3 (var ski)
  "λ² x. x ≡ I
Standard bracket abstraction rule."
  (when (eq var ski)
    :I))

(defun λ²-rule-4 (var ski)
  "λ² x. (M x) ≡  M, (x not in M)
This holds because the first form is just a so called lambda-wrapper around 'b,
which is eventually going to be itself a λ-abstraction."
  (when (and (listp ski)
	     (eq var (second ski))
	     (not (member var (ski-free-variables (first ski)))))
    (first ski)))

(defun λ²-rule-5 (var ski)
  "λ² x. ((x M) x) ≡ λ² x. ((((S S) K) x) M)
Avoids introduction of two Is."
  (when (and (listp ski)
	     (listp (first ski))
	     (eq var (first (first ski)))
	     (eq var (second ski)))
    (let ((M (second (first ski))))
      (λ² var `((((:S :S) :K) ,var) ,M)))))

(defun λ²-rule-6 (var ski)
  "λ² x. (M (N L)) ≡ λ² x. (((S (λ² x. M)) N) L), (M, N combinators)
Prevents occurrences of x in L from becoming too deeply nested."
  (when (and (listp ski)
	     (listp (second ski))
	     (ski-combinator-p (first ski))
	     (ski-combinator-p (first (second ski))))
    (destructuring-bind (M (N L)) ski
      (λ² var `(((:S ,(λ² var M)) ,N) ,L)))))

(defun λ²-rule-7 (var ski)
  "λ² x. ((M N) L) ≡ λ² x. (((S M) (λ 2 x. L)) N), (M, L combinators)
Prevents occurrences of x in N from becoming too deeply nested."
  (when (and (listp ski)
	     (listp (first ski))
	     (ski-combinator-p (first (first ski)))
	     (ski-combinator-p (second ski)))
    (destructuring-bind ((M N) L) ski
      (λ² var `(((:S ,M) ,(λ² var L)) ,N)))))

(defun λ²-rule-8 (var ski)
  "λ² x. ((M L) (N L)) ≡ λ² x. (((S M) N) L), (M, N combinators)
Abstracts an entire expression L to avoid duplication."
  (when (and (listp ski)
	     (listp (first ski))
	     (listp (second ski))
	     (equal (second (first ski))
		    (second (second ski)))
	     (ski-combinator-p (first (first ski)))
	     (ski-combinator-p (first (second ski))))
    (destructuring-bind ((M L) (N L%)) ski
      (λ² var `(((:S ,M) ,N) ,L)))))

(defun λ²-rule-9 (var ski)
  "λ² x. (M N) ≡ ((S (λ² x. M)) (λ² x. N))"
  (when (listp ski)
    `((:S ,(λ² var (first ski))) ,(λ² var (second ski)))))

(defun λ² (var ski)
  "Compact bracket abstraction from [Section 3.2; Tro18]."
  (or (λ²-rule-1 var ski)
      (λ²-rule-2 var ski)
      (λ²-rule-3 var ski)
      (λ²-rule-4 var ski)
      (λ²-rule-5 var ski)
      (λ²-rule-6 var ski)
      (λ²-rule-7 var ski)
      (λ²-rule-8 var ski)
      (λ²-rule-9 var ski)))

;; Debugging helpers

(defun trace-λ²-rules ()
  (trace λ²-rule-1 λ²-rule-2 λ²-rule-3 λ²-rule-4 λ²-rule-5 λ²-rule-6 λ²-rule-7
	 λ²-rule-8 λ²-rule-9))

(defun untrace-λ²-rules ()
  (untrace λ²-rule-1 λ²-rule-2 λ²-rule-3 λ²-rule-4 λ²-rule-5 λ²-rule-6 λ²-rule-7
	   λ²-rule-8 λ²-rule-9))

;;; Binary ski-calculus

;; From [Section 3.1; Tro18].
;; Here I denote terms in the binary ski-calculus representation with the
;; variable BINARY.

(defun ski->binary-ski (ski-combinator)
  "This is only defined for closed ski-terms, that is, ski-combinators"
  (assert (ski-combinator-p ski-combinator))
  (cond ((eq :S ski-combinator)
	 '(0 0))
	((eq :K ski-combinator)
	 '(0 1))
	((eq :I ski-combinator)
	 (ski->binary-ski '((:S :K) :K)))
	((^application-p ski-combinator)
	 (append '(1)
		 (ski->binary-ski (first ski-combinator))
		 (ski->binary-ski (second ski-combinator))))))

(defun get-next-code (binary) ;=> '(code rest)
  (when binary
    (cond ((and (= 0 (first binary))
		(= 0 (second binary)))
	   (list :S (subseq binary 2)))
	  ((= 0 (first binary))
	   (list :K (subseq binary 2)))
	  ;; application
	  ((= 1 (first binary))
	   (destructuring-bind (operation rest-1)
	       (get-next-code (rest binary))
	     (destructuring-bind (operand rest-2)
		 (get-next-code rest-1)
	       (list (list operation operand)
		     rest-2)))))))

(defun add-I (ski)
  "Replaces the subterm '((:S :K) :K) with :I."
  (do-subterms ski
    'add-I
    (lambda (subterm)
      (if (equal subterm '((:S :K) :K))
	  :I
	  subterm))))

(defun binary-ski->ski (binary)
  (destructuring-bind (code rest)
      (get-next-code binary)
    (if rest
	(error "This binary string does not represent a ski term: ~a
~&partial code: ~a~%unparsed rest: ~a.~%"
	       binary code rest)
	(add-I code))))

(defun binary-ski-size (ski)
  (length (ski->binary-ski ski)))
