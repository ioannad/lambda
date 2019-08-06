;;;;test/ui.lisp

(in-package :lambda.test)

(defun test-global-variables (encodings-var)
  (assert (not (null encodings-var)))
  (loop for (name . encoding) in encodings-var
     do (assert (^variable-p name))
     do (test-is-combinator encoding)))

(defun test-prettify-term-properties (term)
  (assert (^term-p (prettify-term term '(a s d f g h j k o i u))))
  (destructuring-bind (free bound) (^variables term)
    (assert (null (intersection free bound)))))

(defun test-pretty-print-manual ()
  (assert (^eq (lambda.ui::print-term  '(λ x (λ x y)))
	       '(Λ A (Λ B Y))))
  (assert (equal (lambda.ui::print-term '(:if a))
		 '(":IF" A)))
  (assert (equal (lambda.ui::pretty-print '((:or a) b) :stream nil)
		 "((:or a) b)")))	  
;; repl-base

(defun test-action-repl-base-properties (term)
  (assert (^term-p (lambda.ui::action-repl-base term))))

(defun test-action-repl-base-manual ()
  (assert (equal (lambda.ui::action-repl-base '((:or a) b))
		 '((A A) B)))
  (assert (equal (lambda.ui::action-repl-base '(((:if :true) a) b))
		 'A))
  (assert (equal (lambda.ui::action-repl-base '((:and a) ((:or :false) c)))
		 '((A C) A))))
;; repl church

(defun test-action-repl-church-properties (church-term)
  (assert (^term-p (encode-church-term
		    (lambda.ui::action-repl-church
		     (encode-church-term church-term))))))

(defun test-action-repl-church-manual ()
  (assert (= 26 (lambda.ui::action-repl-church
		 (encode-church-term '(:A-1 27)))))
  (assert (= 9 (lambda.ui::action-repl-church
		(encode-church-term '((:A+ 2) 7)))))
  (assert (= 24 (lambda.ui::action-repl-church
		 (encode-church-term '(:A-factorial 4))))))

;; transformations viewer

(defun test-transformations-viewer-manual (input output)
  (assert (equal (lambda.ui::pretty-print
		  (lambda.ui::print-term
		   (lambda.ui::validator-transformations-viewer
		    input :warnp nil))
		  :stream nil)
		 output)))

;; all of the above

(defun test-ui (terms &key verbosep)
  (mapcar 'test-global-variables (list *base-encodings*
				       *church-encodings*))
  (mapcar 'test-prettify-term-properties terms)
  (test-pretty-print-manual)
  (mapcar 'test-action-repl-base-properties terms)
  (let ((church-terms (list '(:A+1 3)
			    '(((:if (:Azero? 0)) 1) foo))))
    (mapcar 'test-action-repl-church-properties church-terms))
  (test-action-repl-church-manual)
  (let ((input-1  ':if)
	(output-1 "(λ a (λ b (λ c ((a b) c))))")
	(input-2  '(:eval (((:if :true) a) b)))
	(output-2 "(λ a (λ b a))")
	(input-3  '(:eval ((:and a) ((:or :false) b))))
	(output-3 "(λ a (λ b ((a b) a)))"))
    (mapcar 'test-transformations-viewer-manual
	    (list input-1 input-2 input-3)
	    (list output-1  output-2  output-3)))
  (when verbosep
    (format *standard-output* "~&UI tests passed.~%")))
