;;;;test/transformations.lisp

(in-package :lambda.test)

;;; de Bruijn transformations

(defun manual-test-standard->de-bruijn (term-bruijn)
  "The free variables of your translation should be identical for this to work."
  (destructuring-bind (term . db-term) term-bruijn
    (let ((translated (standard->de-bruijn term)))
      (assert (^eq translated db-term)))))

(defun test-standard->de-bruijn (term)
  (let ((translated (standard->de-bruijn term)))
    (assert (db-term-p translated))
    (assert (set-equal (db-free-variables translated)
		       (^free-variables term)))))

(defun test-de-bruijn-transformations ()
  (let ((terms-bruijns (list
			(cons '(^ x (^ y (z (x y))))
			      '(^   (^   (z (1 0)))))
			(cons '(^ a (b (^ b ((^ c (a (b (^ c c)))) c))))
			      '(^   (b (^   ((^   (2 (1 (^   0)))) c))))))))
    (mapcar 'manual-test-standard->de-bruijn terms-bruijns)
    (mapcar 'test-standard->de-bruijn
	    (mapcar 'car terms-bruijns))))

;;;; SKI-calculus
;;; Functions in transformations-ski.lisp
;; λ² is tested by its results in standard->ski

(defun test-standard->ski->standard (term)
  (assert (lambda.transformations::ski-term-p
	   (standard->ski term)))
  (assert (^term-p (ski->standard (standard->ski term))))
  (assert (^eq (^reduce-normal term)
	       (^reduce-normal
		(ski->standard
		 (standard->ski term))))))

(defun test-Tromp-Y-dash-dash-claim ()
  (let ((Y-- '((λ x (λ y ((x y) x))) (λ y (λ x (y ((x y) x))))))
	(ski-Y-- '(((:S :S) :K) ((:S (:K ((:S :S) (:S ((:S :S) :K))))) :K))))
    (assert (equal (standard->ski Y--)
		   ski-Y--))))

;; tests for the binary representation

(defun test-ski->binary-ski->ski (term)
  (let* ((closed (^closure term))
	 (ski (standard->ski closed)))
    (assert (equal ski
		   (binary-ski->ski
		    (ski->binary-ski ski))))))

(defun test-ski-Tromp-binary-claim ()
  "From [page 8; Tro18], with :S and :K switched."
  (assert (equal (ski->binary-ski '(:K ((:S :K) :K)))
		 '(1 0 1 1 1 0 0 0 1 0 1))))
   
(defun test-ski-Tromp-binary-size-claims ()
  "Taken from [Section 3.2; Tro18]."
  (let ((y   '(λ f ((λ x (x x)) (λ x (f (x x))))))
	(y-  '((λ z (z z)) (λ z (λ f (f ((z z) f))))))
	(y-- '((λ x (λ y ((x y) x))) (λ y (λ x (y ((x y) x))))))
	(omega '((λ x (x x)) (λ x (x x)))))
    (assert (= 65 (lambda.transformations::binary-ski-size
		   (standard->ski y))))
    (assert (= 59 (lambda.transformations::binary-ski-size
		   (standard->ski y-))))
    (assert (= 35 (lambda.transformations::binary-ski-size
		   (standard->ski y--))))
    (assert (= 41 (lambda.transformations::binary-ski-size
		   (standard->ski omega))))))

	  
(defun test-all-transformations-ski (terms)
  "All tests related to ski transformations"
  (mapcar 'test-standard->ski->standard terms)
  (test-Tromp-Y-dash-dash-claim)
  (mapcar 'test-ski->binary-ski->ski terms)
  (test-ski-Tromp-binary-claim)
  (test-ski-Tromp-binary-size-claims))


;;; All transformations.lisp tests 

(defun test-transformations (terms &key verbosep)
  (mapcar 'test-standard->de-bruijn terms)
  (test-de-bruijn-transformations)
  (test-all-transformations-ski terms)
  
  (when verbosep
    (format *standard-output* "~&TRANSFORMATIONS tests passed.~%")))

