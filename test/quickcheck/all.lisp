;;;;test/quickcheck/all.lisp

;;; All the quickchecks

(in-package :lambda.quickcheck)

(defun run-quickcheck ()
  (let ((depth 27) ; The maximum depth of a ^term in the tests.
	(bound 5)  ; A bound on new (some-^variables) to generate.
	(verbosep NIL)) ; Control test verbosity.
    (setf *break-on-failure* T)
    (setf *loud* nil) ; More verbosity contol
    (quickcheck
      ;; syntax-standard
      (quickcheck-predicates depth verbosep)
      
      (let ((term-generator         (lambda () (a-^term depth)))
	    (term%-generator        (lambda () (a-^term depth)))
	    (term%%-generator       (lambda () (a-^term depth)))
	    (binding-generator      (lambda () (a-binding-^abstraction depth))))

	(when verbosep
	  (print "Generating three terms"))
	(for-all (term term% term%%)
	  (when verbosep
	    (format *error-output*
		    "~&Generating:~%term   ~a~%term%  ~a~%term%% ~a~%"
		    term term% term%%))
	  (check-syntax-closures (list term term% term%%))
	  
	  (let ((free (^free-variables term)))
	    (when free
	      (check-syntax-free-variables free)
	      (check-transformations term free)
	      (check-substitution term free bound depth)
	      (check-reducers term term% free)
	      (check-renamed-term term free)))
	    
	  (check-interpreter-encodings term term% term%%)
	  (check-ui (list term term% term%%))
	  (check-compiler (list term term% term%%)))
	
	
	;; specialized tests for bound abstractions, i.e., abstractions whose
	;; VARIABLE actually appears in the BODY.
	(when verbosep
	  (print "Generating ^abstractions with bound variables"))
	(for-all (binding)
	  (destructuring-bind (^% variable body) binding
	    (check-syntax-standard-binding ^% variable body binding)
	    (check-renames-equality variable body)))))))
	    
