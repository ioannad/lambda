;;;;decodings.lisp

(in-package :lambda.decodings)

;;; Working with encodings

(defun get-encoding (name encodings) ; => ^term
  (cdr (find name
	     encodings
	     :test (lambda (name assignment)
		     (equal name (car assignment))))))

(defun encoding-p (encoding encodings) ; => encoding-name
  (car (find encoding
	     encodings
	     :test (lambda (encoding assignment)
		     (^eq encoding (cdr assignment))))))

;;; Decodings

(defun decode-encoding (encoding encodings) ; => ^term
  (or (encoding-p encoding encodings)
      (progn (format *standard-output*
		     "~%Not a defined encoding: ~a~%"
		     encoding)
	     encoding)))

(defun decode-encodings-in-term (term encodings) 
  (or (encoding-p term encodings)
      (^ecase term
	      ((:variable term)
	       ((operation operand) :application
		(list (decode-encodings-in-term operation encodings)
		      (decode-encodings-in-term operand encodings)))
	       ((var body) :abstraction
		(list 'λ var (decode-encodings-in-term body encodings)))))))

;;; Decoding numerals

;; Church numerals

(defun count-applications (operation operand term &optional (count 0))
  (if (and (^application-p term)
	   (equal operation (first term)))
      (count-applications operation operand (second term) (1+ count))
      (values count term)))	   

(defun decode-church-numeral (term)
  "Returns NIL if TERM is not a Church numeral."
  (when (and (^abstraction-p term)
	     (^abstraction-p (third term)))
    (destructuring-bind (λ% f (λ%% x applications)) term
      (declare (ignorable λ% λ%%))
      (multiple-value-bind (count rest)
	  (count-applications f x applications)
	(when (equal x rest)
	  count)))))
  
(defun decode-church-term (term encodings)
  (let ((partly-decoded (decode-encodings-in-term term encodings)))
    (do-abstractions partly-decoded
      (lambda (f body)
	(let ((maybe-numeral (decode-church-numeral term)))
	  (if (numberp maybe-numeral)
	      maybe-numeral
	      (list 'λ f (decode-church-term body encodings)))))
      #'decode-church-term encodings)))

;; Barendregt numerals 

(defun count-nested-pairs (bruijn-term &optional (count 0))
  (if (equal bruijn-term :id)
      count
      (when (and (^symbol-p (first bruijn-term))
		 (equal '(0 :FALSE)
			(first (second bruijn-term))))
	(count-nested-pairs (second (second bruijn-term))
			    (1+ count)))))
	
(defun decode-barendregt-numeral (term environment)
  "Returns NIL if the TERM is not a Barendregt numeral."
  (let ((bruijn-decoded (standard->de-bruijn
			 (decode-encodings-in-term term environment))))
    (count-nested-pairs bruijn-decoded)))
