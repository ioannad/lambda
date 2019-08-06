;;;;test/quickcheck/ui.lisp

(in-package :lambda.quickcheck)

(defun check-ui (terms)
  (loop for term in terms
     do (is ^term-p (lambda.ui::action-repl-base term))
     do (is ^term-p (lambda.ui::print-term
		     (lambda.ui::validator-transformations-viewer
		      term :warnp nil)))))


