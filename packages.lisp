;;;;packages.lisp

(defpackage :lambda.syntax-standard
  (:use #:cl)
  (:export #:^ #:λ #:l ; TODO: consider using keywords instead (in TODO list)
	   #:^symbol-p
	   #:^variable-p
	   #:^application-p
	   #:^abstraction-p
	   #:^term-p
	   #:^ecase
	   #:do-applications
	   #:do-abstractions
	   #:do-subterms
	   #:^free-variables
	   #:^variables
	   #:^bound-variables
	   #:^closure))

(defpackage :lambda.syntax-de-bruijn
  (:use #:cl
	#:lambda.syntax-standard)
  (:export #:db-abstraction-p
	   #:db-abstraction-body
	   #:db-term-p
	   #:db-free-variables))

(defpackage :lambda.syntax-ski
  (:use #:cl
	#:lambda.syntax-standard)
  (:export #:ski-term-p
	   #:ski-free-variables
	   #:ski-combinator-p))

(defpackage :lambda.transformations
  (:use #:cl
	#:lambda.syntax-standard
	#:lambda.syntax-de-bruijn
	#:lambda.syntax-ski)
  (:export #:standard->de-bruijn
	   #:standard->common-lisp
	   #:standard->ski
	   #:ski->standard
	   #:ski->binary-ski
	   #:binary-ski->ski))

(defpackage :lambda.renames
  (:use #:cl
	#:lambda.syntax-standard)
  (:export #:rename
	   #:α-convert
	   #:prettify-^term))

(defpackage :lambda.equality
  (:use #:cl
	#:lambda.transformations)
  (:export #:^eq))

(defpackage :lambda.substitution
  (:use #:cl
	#:lambda.syntax-standard
	#:lambda.renames)
  (:export #:assign
	   #:^substitute
	   #:^substitute-environment
	   #:full-α-conversion
	   #:prettify-term)) ; this is not a word

(defpackage :lambda.reducers
  (:use #:cl
	#:lambda.syntax-standard
	#:lambda.renames
	#:lambda.equality
	#:lambda.substitution)
  (:export #:^reduce-step
	   #:^reduce
	   #:^reduce-lazy-step
	   #:^reduce-normal-step
	   #:^reduce-applicative-step
	   #:^reduce-catch-loop
	   ;; shorthands
	   #:^reduce-normal
	   #:^reduce-normal-catch-loop))

(defpackage :lambda.encodings
  (:use #:cl
	#:lambda.syntax-standard
	#:lambda.transformations
	#:lambda.renames
	#:lambda.equality
	#:lambda.substitution)
  (:export #:basic-combinators
	   #:recursing-combinators
	   #:church-numeral
	   #:church-arithmetic
	   #:encode-church-term))

(defpackage :lambda.decodings
  (:use #:cl
	#:lambda.syntax-standard
	#:lambda.transformations
	#:lambda.equality)
  (:export #:decode-encodings-in-term
	   #:decode-church-term))
	  
(defpackage :lambda.interpreter
  (:use #:cl
	#:lambda.syntax-standard
	#:lambda.substitution
	#:lambda.reducers
	#:lambda.encodings
	#:lambda.decodings)
  (:export #:^eval
	   #:^eval-normal
	   #:^eval-step
	   #:^eval-normal-steps
	   #:^eval-catch-loop
	   #:^eval-normal-catch-loop
	   #:base-encodings
	   #:church-encodings
	   #:all-encodings
	   #:^interpret))

(defpackage :lambda.ui
  (:use #:cl
	#:lambda.syntax-standard
	#:lambda.transformations
	#:lambda.renames
	#:lambda.substitution
	#:lambda.encodings
	#:lambda.decodings
	#:lambda.interpreter)
  (:export #:*base-encodings*
	   #:*church-encodings*
	   #:pretty-print
	   #:main))

(defpackage :lambda.compiler
  (:use #:cl
	#:lambda.syntax-standard
	#:lambda.transformations
	#:lambda.substitution
	#:lambda.interpreter)
  (:export #:^compile))

;; A package with everything as a playground

(defpackage :lambda
  (:use #:cl
	#:lambda.syntax-standard
	#:lambda.syntax-de-bruijn
	#:lambda.syntax-ski
	#:lambda.transformations
	#:lambda.renames
	#:lambda.equality
	#:lambda.substitution
	#:lambda.reducers
	#:lambda.encodings
	#:lambda.decodings
	#:lambda.interpreter
	#:lambda.ui
	#:lambda.compiler))


;;; Test packages

(defpackage :lambda.test
  (:use #:cl
	#:lambda.syntax-standard
	#:lambda.syntax-de-bruijn
	#:lambda.syntax-ski
	#:lambda.transformations
	#:lambda.renames
	#:lambda.equality
	#:lambda.substitution
	#:lambda.reducers
	#:lambda.encodings
	#:lambda.decodings
	#:lambda.interpreter
	#:lambda.ui
	#:lambda.compiler)
  (:export #:test-all))

(defpackage :lambda.quickcheck
  (:use #:cl
	#:cl-quickcheck
	#:lambda.syntax-standard
	#:lambda.syntax-de-bruijn
	#:lambda.transformations
	#:lambda.renames
	#:lambda.equality
	#:lambda.substitution
	#:lambda.reducers
	#:lambda.encodings
	#:lambda.decodings
	#:lambda.interpreter
	#:lambda.ui
	#:lambda.compiler)
  (:export #:run-quickcheck))
