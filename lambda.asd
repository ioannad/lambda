;;;;lambda.asd

(defsystem "lambda"
  :description "An implementation of untyped lambda (^) calculus in several representations, with a UI, interpreter, and compiler."
  :author "Ioanna M. Dimitriou H. (ioa) <ioa+lambda@pr0.tips>"
  :license  "Apache Licence 2.0"
  :version "0.0.1"
  :depends-on ("cl-quickcheck")
  :components ((:file "packages")
	       (:file "syntax-standard"
		      :depends-on ("packages"))
	       (:file "syntax-de-bruijn"
		      :depends-on ("packages"
				   "syntax-standard"))
	       (:file "syntax-ski"
		      :depends-on ("packages"
				   "syntax-standard"))
	       (:file "transformations-ski"
		      :depends-on ("packages"
				   "syntax-standard"
				   "syntax-ski"))
	       (:file "transformations"
		      :depends-on ("packages"
				   "syntax-standard"
				   "syntax-de-bruijn"
				   "syntax-ski"
				   "transformations-ski"))
	       (:file "renames"
		      :depends-on ("packages"
				   "syntax-standard"))
	       (:file "equality"
		      :depends-on ("packages"
				   "transformations"))
	       (:file "substitution"
		      :depends-on ("packages"
				   "syntax-standard"
				   "renames"))
	       (:file "reducers"
		      :depends-on ("packages"
				   "syntax-standard"
				   "substitution"))
	       (:file "encodings"
		      :depends-on ("packages"
				   "syntax-standard"
				   "transformations"
				   "renames"
				   "equality"
				   "substitution"))
	       (:file "decodings"
		      :depends-on ("packages"
				   "syntax-standard"
				   "transformations"
				   "encodings"
				   "equality"))
	       (:file "interpreter"
		      :depends-on ("packages"
				   "syntax-standard"
				   "substitution"
				   "reducers"
				   "encodings"
				   "decodings"))
	       (:file "ui-texts"
		      :depends-on ("packages"))
	       (:file "ui"
		      :depends-on ("packages"
				   "syntax-standard"
				   "transformations"
				   "renames"
				   "substitution"
				   "encodings"
				   "decodings"
				   "interpreter"
				   "ui-texts"))
	       (:file "compiler"
		      :depends-on ("packages"
				   "syntax-standard"
				   "transformations"
				   "interpreter"))
	      
	       ;; test suite

	       ;; Manual tests, files in package lambda.test

	       (:file "test/helpers")
	       (:file "test/syntax-standard"
		      :depends-on ("packages"
				   "test/helpers"
				   "syntax-standard"))
	       (:file "test/syntax-de-bruijn"
		      :depends-on ("packages"
				   "test/helpers"
				   "syntax-standard"
				   "syntax-de-bruijn"))
	       (:file "test/syntax-ski"
		      :depends-on ("packages"
				   "syntax-ski"
				   "test/helpers"))
	       (:file "test/transformations"
		      :depends-on ("packages"
				   "test/helpers"
				   "syntax-standard"
				   "syntax-de-bruijn"
				   "syntax-ski"
				   "transformations-ski"
				   "transformations"))
	       (:file "test/renames"
		      :depends-on ("packages"
				   "syntax-standard"
				   "renames"))
	       (:file "test/equality"
		      :depends-on ("packages"
				   "equality"))
	       (:file "test/substitution"
		      :depends-on ("packages"
				   "substitution"))
	       (:file "test/reducers"
		      :depends-on ("packages"
				   "reducers"))
	       (:file "test/encodings"
		      :depends-on ("packages"
				   "syntax-standard"
				   "encodings"
				   "decodings"
				   "interpreter"
				   "ui"))
	       (:file "test/decodings"
		      :depends-on ("packages"
				   "syntax-standard"
				   "encodings"
				   "decodings"
				   "interpreter"
				   "ui"))
	       (:file "test/interpreter"
		      :depends-on ("packages"
				   "substitution"
				   "encodings"
				   "decodings"
				   "interpreter"
				   "ui"))
	       (:file "test/ui"
		      :depends-on ("packages"
				   "syntax-standard"
				   "test/encodings"
				   "ui"))
	       (:file "test/compiler"
		      :depends-on ("packages"
				   "compiler"
				   "encodings"))
	       (:file "test/all"
		      :depends-on ("packages"
				   "test/syntax-standard"
				   "test/syntax-de-bruijn"
				   "test/syntax-ski"
				   "test/transformations"
				   "test/renames"
				   "test/equality"
				   "test/substitution"
				   "test/reducers"
				   "test/encodings"
				   "test/decodings"
				   "test/interpreter"
				   "test/ui"
				   "test/compiler"))
	       
	       ;; Files in package :lambda.quickcheck

	       (:file "test/quickcheck/helpers"
		      :depends-on ("packages"))
	       (:file "test/quickcheck/generators"
		      :depends-on ("packages"
				   "test/quickcheck/helpers"
				   "syntax-standard"))
	       (:file "test/quickcheck/syntax-standard"
		      :depends-on ("packages"
				   "syntax-standard"
				   "test/quickcheck/helpers"
				   "test/quickcheck/generators"))
	       (:file "test/quickcheck/transformations"
		      :depends-on ("packages"
				   "syntax-standard"
				   "syntax-de-bruijn"
				   "transformations-ski"
				   "transformations"
				   "test/quickcheck/helpers"))
	       (:file "test/quickcheck/renames-equality"
		      :depends-on ("packages"
				   "renames"
				   "equality"))
	       (:file "test/quickcheck/substitution"
		      :depends-on ("packages"
				   "syntax-standard"
				   "substitution"
				   "test/quickcheck/helpers"))
	       (:file "test/quickcheck/reducers"
		      :depends-on ("packages"
				   "reducers"))
	       (:file "test/quickcheck/interpreter-encodings"
		      ;; Includes examples and lemmas from [Barendregt 1994].
		      :depends-on ("packages"
				   "interpreter"
				   "encodings"))
	       (:file "test/quickcheck/ui"
		      :depends-on ("packages"
				   "ui"))
	       (:file "test/quickcheck/compiler"
		      :depends-on ("packages"
				   "compiler"))
	       (:file "test/quickcheck/all"
		      :depends-on ("packages"
				   "syntax-standard"
				   "test/quickcheck/helpers"
				   "test/quickcheck/generators"
				   "test/quickcheck/syntax-standard"
				   "test/quickcheck/transformations"
				   "test/quickcheck/renames-equality"
				   "test/quickcheck/reducers"
				   "test/quickcheck/interpreter-encodings"
				   "test/quickcheck/ui"
				   "test/quickcheck/compiler"))))
