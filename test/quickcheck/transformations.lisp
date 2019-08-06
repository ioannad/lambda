;;;;test/quickcheck/transformations.lisp

(in-package :lambda.quickcheck)

;; Other syntaxes apart from syntax-standard are not being quickchecked because
;; they require different generators, and because they should only be created by
;; the transformations in transform.lisp.

(defun check-standard->de-bruijn (term free)
  (let ((db-term (standard->de-bruijn term)))
    (is db-term-p db-term)
    (is set-equal
	(db-free-variables db-term)
	free)))

;; standard->common-lisp is checked in test/quickcheck/compiler.lisp

;;; Tests involving ski-calculus transformations

;; What can cause a difference between a reduced closed term, and its translated
;; to and from a ski term, is λ²'s rule 4, which replaces '(λ a (b a)) with 'b.
;; This holds because the first form is just a so called lambda-wrapper around
;; 'b, which is eventually going to be a closed term.

(defun lambda-unwrap (term)
  (do-abstractions term 
    (lambda (var body)
      (if (and (^application-p body)
	       (eq var (second body))
	       (not (member var (^free-variables (first body)))))
	  (lambda-unwrap (first body))
	  `(λ ,var ,(lambda-unwrap body))))
    'lambda-unwrap))

(defun check-standard->ski->standard (term)
  (let ((closed (^closure term)))
    (is ^eq
	(lambda-unwrap (^reduce-normal closed))
	(lambda-unwrap (^reduce-normal (ski->standard
					(standard->ski closed)))))))
    
(defun check-ski->binary-ski->ski (term)
  (let* ((closed (^closure term))
	 (ski (standard->ski closed)))
    (is equal
	ski
	(binary-ski->ski
	 (ski->binary-ski ski)))))


(defun check-transformations (term free)
  (check-standard->de-bruijn term free)
  (check-standard->ski->standard term)
  (check-ski->binary-ski->ski term))

