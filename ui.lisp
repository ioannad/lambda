;;;;ui.lisp

(in-package :lambda.ui)

;;; User Interface for λ calculus.
;;
;; Includes:
;;
;; - Transformation viewer: input a term and see all its available
;;   representations.
;; - Two Read Print Eval Loops (REPL)
;;   + "base": Allows :ID :TRUE :FALSE :S :AND :OR :IF :Y :OMEGA
;;   + "church": Allows base, non-negative integers, and the numeric functions
;;     :A+ :A* :Aexp :Azero? :A+1 :A-1 :A-factorial (plus, times, exponent,
;;     is-zero predicate, successor, predecessor, and factorial, respectively).
;;-------------------------------------------------------------------------

;;; Global variables

;; to hold simplified encodings of combinators, for each of the UI REPLs.

(defvar *base-encodings* NIL)

(defvar *church-encodings* NIL)

(setf *base-encodings* (base-encodings))
(setf *church-encodings* (church-encodings))

;;; User interaction

(defun prompt (string
	       &key (stream *standard-output*))
  (let ((*read-eval* nil))
    (format stream "~&~a " string)
    (finish-output)
    (handler-case (read nil 'eof nil)
      (error (e) nil))))

;; Pretty printing

(defun print-assignment (assignment ; makes you think of structures ....
			 &key (stream *standard-output*))
  (format stream "~(~S~) ⟶ ~(~a~)~%"
	  (car assignment)
	  (print-term (cdr assignment))))

(defun print-environment (environment
			  &key (stream *standard-output*))
  (loop for assignment in environment
     do (print-assignment assignment :stream stream)))

(defun make-symbols (prefix amount)
  (loop for i from 1 to amount
     collect (intern (format nil "~a~a" prefix i))))

(defun print-term (term &aux
			  (names '(a b c d e f g h i j k m n
				   o p q r s t u v w x y z)))
  (destructuring-bind (free bound) (^variables term)
    (delete-if (lambda (name) (member name free))
	       names)
    (when (> (length bound)
	     (length names))
      (setf names (make-symbols "x" (length bound))))
    (prettify-term term names)))

(defun pretty-print (thing
		     &key (stream *standard-output*))
  "Pretty print things with variables from other packages, for example terms, 
church terms."
  (if (handler-case (^term-p thing)
	(error () ))
      (format stream "~(~a~)" (print-term thing))
      (format stream "~a" thing)))

;;; Help functions

;; The help functions including long text, are defined in ui-texts.lisp

(defun help (stream)
  (initial-help stream)
  (format-warnings stream))

(defun help-transformation-viewer (stream)
  (initial-help-transformation-viewer stream)
  (format-warnings stream)
  (format stream "~%~%Example inputs:~%")
  (format stream
	  ":or~%(((:if :true) a) b)~%:eval ((:and a) ((:or :false) b))~%~%"))

(defun help-base (stream)
  (initial-help-base stream)
  (format-warnings stream)
  (format stream "~%~%Example inputs:~%
(((:if :true) a) b)~%((:and a) ((:or :false) c))~%~%"))

(defun help-church (stream)
  (initial-help-church stream)
  (format-warnings stream)
  (format
   stream
   "~%~%If you try anything larger than (:A-factorial 4), prepare to wait.~%")
  (format stream "~%~%Example inputs:~%((:A+ 2) 7)~%(:A+1 27)~%~%"))

(defun help-back-exit (stream)
  (format stream "~&This UI is not case sensitive.~%~%")
  (format stream "~&To go to the previous menu, input :back")
  (format stream "~%To exit, input :exit~%~%"))

;;; UI parts

;; The UI loop

(defun ui-validate-action-print (user-input validator action printer stream)
  (let ((validated (handler-case (funcall validator user-input)
		     (error (e) (format stream "~&Error caught: ~a~%" e)))))
    (funcall printer
	     (funcall action validated)
	     :stream stream)))

(defun ui-loop (name validator action printer stream
		&key (debug nil))
  (let ((user-input (prompt (format nil " λ.~a >>   " name)
			    :stream stream)))
    (when debug
      (format stream "~%   Debug info: User-input = ~S~%" user-input))
    (cond ((equal user-input :back)
	   (main))
	  ((equal user-input :exit)
	   (exit-ui stream))
	  (T (ui-validate-action-print
	      user-input validator action printer stream)
	     (ui-loop
	      name validator action printer stream)))))

;; REPL-base

(defun action-repl-base (validated-input)
  (^interpret validated-input *base-encodings*
	      :decoding #'decode-encodings-in-term))

(defun repl-base (stream)
  (help-base stream)
  (help-back-exit stream)
  (ui-loop "base"
	   #'^term-p
	   #'action-repl-base
	   #'pretty-print
	   stream))

;; REPL-church

(defun action-repl-church (validated-input)
  (^interpret validated-input *church-encodings*
	      :decoding  #'decode-church-term))

(defun repl-church (stream)
  (help-church stream)
  (help-back-exit stream)
  (ui-loop "church"
	   #'encode-church-term
	   #'action-repl-church
	   #'pretty-print
	   stream))

;; Transformations Viewer

(defun close-warn (term free &key (warnp t))
  (let ((closed (^closure term :free-variables free)))
    (when warnp
      (format *error-output*
	      "~&Warning! You enter the open λ term: ~s~%"
	      term)
      (format *error-output*
	      "Closing the term to:                ~s~%"
	      closed))
    closed))

(defun validator-transformations-viewer (user-input &key (warnp t) &aux evalp)
  (let* ((maybe-term (if (and (listp user-input)
			      (equal :eval (first user-input)))
			 (progn (setf evalp T)
				(second user-input))
			 user-input))
	 ;; Validation happens by ^free-variables
	 (free (set-difference (^free-variables maybe-term)
			       (mapcar 'car *base-encodings*))))
    (list evalp
	  (if free
	      (close-warn maybe-term free :warnp warnp)
	      maybe-term))))

(defun print-transformations (term &key stream)
  (format stream
	  "~&
* standard-syntax with primitives:        ~a
* standard-syntax without primitives:     ~(~a~)
* de Bruijn representation:               ~(~a~)
* Common Lisp representation:             ~(~a~)~%~%"
	  (pretty-print
	   (decode-encodings-in-term term (reverse *base-encodings*))
	   :stream nil)
	  term
	  (standard->de-bruijn term)
	  (standard->common-lisp term)))

(defun action-transformations-viewer (validated)
  (print-term
   (destructuring-bind (evalp term) validated
     (if evalp
	 (^eval-normal term *base-encodings*)
	 (^substitute-environment term *base-encodings*)))))

(defun transformation-viewer (stream)
  (help-transformation-viewer stream)
  (help-back-exit stream)
  (ui-loop "viewer"
	   #'validator-transformations-viewer
	   #'action-transformations-viewer
	   #'print-transformations
	   stream))

;; exit point

(defun exit-ui (stream)
  (format stream "~%Bye.~%"))

;;; Main

;; Entry point

(defun main (&optional (stream *standard-output*))
  (let ((user-input (prompt "
Input which REPL to use:
  3  for the transformation viewer
  2  for the church REPL
  1  for the base REPL
  0  to exit
Anything else will return help instructions.
  ")))
    (case user-input
      (0   (exit-ui stream))
      (1   (repl-base stream))
      (2   (repl-church stream))
      (3   (transformation-viewer stream))
      (otherwise
       (help stream)
       (main)))))

