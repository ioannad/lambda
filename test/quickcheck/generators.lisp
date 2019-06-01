;;;;test/quickcheck/generators.lisp

;;;; Generators for cl-quickcheck and later for benchmarks

;;; A short introduction to cl-quickcheck, at the time being in version 0.4.

;; TODO Add short introduction to cl-quickcheck.

(in-package :lambda.quickcheck)

(defvar *symbols*
  '(q w e r t z u i o p ü a s d f g h j k ö ä y x c v b n m)
  "The symbols to be used when generating λ terms. To make the test objects 
readable, and the probability of bound variables higher.")

(defun a-^symbol ()
  (random-element '(^ λ l)))  

(defun a-^variable () 
  ;;  (let ((sym (a-symbol))) ;; just use a pool of symbols instead 
  (random-element *symbols*))

(defun some-^variables (bound)
  "Always generates at least one ^variable, because (loop from 0 to 0 do X),
does X."
  (let ((n (random bound)))
    (loop
       for i from 0 to n
       collect (a-^variable))))

(defun a-^application (depth)
  (let ((operator (a-^term (1- depth)))
	(operand  (a-^term (1- depth))))
    (list operator operand)))

(defun a-^abstraction (depth)
  (let ((^sym (a-^symbol))
	(var  (a-^variable))
	(body (a-^term (1- depth))))
    (list ^sym var body)))

(defun a-binding-^abstraction (depth)
  (let* ((^sym (a-^symbol))
	 (body (a-^term (1- depth)))
	 (free (^free-variables body)))
    (if (null free)
	(a-binding-^abstraction depth)
	(let ((var  (random-element free)))
	  (list ^sym var body)))))

(defun an-infinite-recursion (depth)
  (let ((x (a-^variable)))
    (if (null depth)
	`((^ ,x (,x ,x)) (^ ,x (,x ,x)))
	(let ((vars (loop for i from 1 to depth
		       collect (format nil "x~a" i))))
	  (read-from-string
	   (format nil
		   "(^ ~a ~{(^ ~a ~} (~a ~a) ~v@{~A~:*~})"
		   x vars x x depth ")"))))))

(defun a-^term (max-depth)
  (if (<= max-depth 0)
      (a-^variable)
      (let ((depth (random max-depth)))
	(funcall
	 (random-element
	  (list (lambda () (a-^application depth))
		(lambda () (a-binding-^abstraction depth))
		(lambda () (a-^abstraction depth))))))))

