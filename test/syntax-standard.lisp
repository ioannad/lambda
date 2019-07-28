;;;;test/syntax-standard.lisp

(in-package :lambda.test)

(defun test-^variable-p ()
  (assert (every '^variable-p '(a d f g2 h foo ba-r :s :foo)))
  (assert (not (some '^variable-p '(^ λ l))))
  (assert (and (^variable-p 'x)
	       (not (^variable-p 'l))
	       (not (^variable-p '^)))))

(defun test-^abstraction-p ()
  (assert (^abstraction-p '(^ x x)))
  (assert (^abstraction-p '(^ x (λ a (a (b x))))))
  ;; shallow predicate
  (assert (^abstraction-p '(λ a "foo"))))

(defun test-^application-p ()
  (assert (^application-p '((l t t) z)))
  (assert (^application-p '("foo" 4))))
  
(defun test-^term-p ()
  (let ((e-1 'x)
	(e-2 '(^ x y))
	(e-3 '((l t t) z))
	(e-4 '(λ a (λ b (c (λ d f))))))
    (assert (every '^term-p
		   (list e-1 e-2 e-3 e-4)))))

(defun test-do-applications ()
  (labels ((get-operation (term)
	     (do-applications term
	       (lambda (operation operand)
		 (get-operation operation))
	       #'get-operation)))
    (assert (equal '(λ d x)
		   (get-operation
		    '(λ d (((x (λ x x)) (λ a (a a))) (λ z (z (z z))))))))))

(defun test-do-abstractions ()
  (labels ((get-body (term)
	     (do-abstractions term
	       (lambda (var body) body)
	       #'get-body)))
    (assert (equal '(((X X) (A A)) (Z (Z Z)))
		   (get-body
		    '(((x (λ x x)) (λ a (a a))) (λ z (z (z z)))))))))

(defun test-do-subterms ()
  (labels ((recur (term) (do-subterms term
			   #'recur (lambda (subterm)
				     (if (eq subterm 'a)
					 'c
					 subterm)))))
    (assert (equal (recur '(a (λ b ((a b) c))))
		   '(C (Λ B ((C B) C)))))))

(defun test-^free-variables ()
  (handler-case (^free-variables '(λ x y z r))
    (error (error)
      (declare (ignore error)))
    (:no-error ()
      (error "~%Pattern matcher in '^free-variables didn't spot this wrong 
pattern!~%")))
  
  (assert (not (lambda.syntax-standard::^free-variable-p
		'x
		'(λ x x))))
  (assert (lambda.syntax-standard::^free-variable-p
	   'y
	   '(λ x (x y))))
  (assert (lambda.syntax-standard::^free-variable-p
	   'z
	   '(λ y (λ x (x ((a (y z)) (y v)))))))
  (assert (not (lambda.syntax-standard::^free-variable-p
		'x
		'(λ y (λ x (x ((a (y z)) (y v)))))))))

(defun test-^bound-variables ()
  (assert (member 'x (^bound-variables '(λ x x))))
  (assert (equal '(y x r)
		 (^bound-variables '(λ y (λ x ((foo (λ r bar)) baz)))))))

(defun test-^closure ()
  (assert (equal (^closure '(^ x (x (y z))))
		 '(λ y (λ z (^ x (x (y z)))))))
  (assert (equal (^closure #1='(^ x (^ y (y x))))
		 #1#)))

(defun test-^closure-properties (term)
  (let ((closed (^closure term)))
    (assert (null (^free-variables closed)))
    (assert (equal (^closure closed)
		   closed))
    (destructuring-bind (free bound) (^variables term)
      (assert (set-equal (union free bound)
			 (^bound-variables closed))))))

(defun test-syntax-standard (terms &key verbosep)
  (test-^variable-p)
  (test-^abstraction-p)
  (test-^application-p)
  (test-^term-p)
  (test-do-abstractions)
  (test-do-applications)
  (test-do-subterms)
  (test-^free-variables)
  (test-^bound-variables)
  (test-^closure)
  (mapcar 'test-^closure-properties terms)
  (when verbosep
    (format *standard-output* "~&SYNTAX-STANDARD tests passed.~%")))
