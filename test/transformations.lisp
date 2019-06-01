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

;;; All transformations.lisp tests 

(defun test-transformations (terms &key verbosep)
  (mapcar 'test-standard->de-bruijn terms)
  (test-de-bruijn-transformations)
  
  (when verbosep
    (format *standard-output* "~&TRANSFORMATIONS tests passed.~%")))

