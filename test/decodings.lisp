;;;;test/decodings.lisp

(in-package :lambda.test)


(defun test-working-with-encodings ()
  (let ((encoding-selection (list '(λ ax (λ bx bx))
				  '(λ ax ((λ bx (ax (bx bx))) (λ xc (ax (xc xc)))))
				  '(λ a (λ b ((a a) b)))))
	(encoding-names     (list :false
				  :Y
				  :or))
	(all-encodings (all-encodings)))
    (assert (every (lambda (selection)
		     (lambda.decodings::encoding-p selection
						   all-encodings))
		   encoding-selection))
    (loop for selection in encoding-selection
       for name in encoding-names
       do (assert (equal (lambda.decodings::decode-encoding
			  selection
			  all-encodings)									
			 name)))
    (assert (equal (decode-encodings-in-term
		    (^substitute-environment '(:id :id) (basic-combinators))
		    all-encodings)
		   '(:id :id)))
    (assert (equal (decode-encodings-in-term
		    (^eval '((:or :true) :false) (basic-combinators))
		    all-encodings)
		   :true))))

(defun test-exercise-2-7 ()
  (let ((basic (basic-combinators)))
    (assert (equal (decode-encodings-in-term
		    (^eval '(:true :id) basic)
		    basic)
		   :FALSE))
    (assert (equal (decode-encodings-in-term
		    (^eval '((:S :true) :true) basic)
		    basic)
		   :ID))))

(defun test-more-decoded ()
  (let ((encodings (all-encodings))
	(church *church-encodings*))
    
    (assert (equal (decode-encodings-in-term
		    (^eval `(:Zero ,(lambda.encodings::barendregt-numeral 0))
			   encodings
			   :use-reduction-strategy '^reduce-normal)
		    encodings)
		   :TRUE))
    (assert (equal :TRUE
		   (^interpret (encode-church-term `(:Azero? 0)) church
			       :decoding #'lambda.decodings::decode-encoding)))
    (assert (equal :FALSE
		   (^interpret (encode-church-term `(:Azero? 1)) church
			       :decoding #'lambda.decodings::decode-encoding)))
    (assert (equal :FALSE
		   (^interpret (encode-church-term `(:Azero? 13)) church
			       :decoding #'lambda.decodings::decode-encoding)))
    (assert (= (^interpret (encode-church-term `(:A+1 13)) church
			   :decoding #'decode-church-term)
	       14))
    (assert (= (^interpret (encode-church-term  `(:A-1 13)) church
			   :decoding #'decode-church-term)
	       12))
    (assert (= 1
	       (^interpret (encode-church-term  `(:A-factorial 1)) church
			   :decoding #'decode-church-term)))
    (assert (= (^interpret (encode-church-term  `(:A-factorial 2)) church
			   :decoding #'decode-church-term)
	       2))
    (assert (= (^interpret (encode-church-term  `(:A-factorial 3)) church
			   :decoding #'decode-church-term)
	       6))
    (assert (= (^interpret (encode-church-term  `(:A-factorial 4)) church
			   :decoding #'decode-church-term)
	       24))
    (assert (^eq (lambda.decodings::get-encoding :S+ encodings)
		 '(λ a (^ b ((b (λ c (λ d d))) a)))))
    (assert (= 9
	       (lambda.decodings::decode-barendregt-numeral
		(^eval-normal `((:B+ ,(lambda.encodings::barendregt-numeral 2))
				,(lambda.encodings::barendregt-numeral 7))
			      encodings)
		encodings)))
    (assert (= 4
	       (lambda.decodings::decode-barendregt-numeral
		(^eval-normal `((:B* ,(lambda.encodings::barendregt-numeral 2))
				,(lambda.encodings::barendregt-numeral 2))
			      encodings)
		encodings)))))

(defun test-decodings (&key verbosep)
  (test-working-with-encodings)
  (test-exercise-2-7)
  (test-more-decoded)
  (when verbosep
    (format *standard-output* "~&DECODINGS tests passed.~%")))
