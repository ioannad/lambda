;;;;ui-texts.lisp

(in-package :lambda.ui)

;;; UI functions which include large texts.

(defun initial-help (stream)
  (format stream "
LAMBDA:  λ, ^, l
================

λ calculus syntax
-----------------
^term ::= ^variable | ^application | ^abstraction
^variable ::= any common-lisp symbol except '^, 'λ, 'l
^application ::= (list ^term ^term)
^abstraction ::= (list ^symbol ^variable ^term)
^symbol ::= '^ | 'λ | 'l


Examples of λ terms
-------------------
(λ a (a c))            in common-lisp notation: (lambda (a) (funcall a c))

((^ a (a c)) (^ a a))  in common-lisp notation: (funcall (lambda (a) (funcall a c))
						         (lambda (a) a))

UI Description
--------------

In the main menu you have the following opions:

  3  for the transformation viewer
  2  for the church REPL
  1  for the base REPL
  0  to exit

Anything else will return help instructions.

Each UI starts with help instructions and example of usage, and each has a set 
of self encoded primitives, such as :ID :TRUE :FALSE :S :AND :OR :IF :Y :OMEGA, 
which you can use when entering a λ term.

Primitives are necessary for humans to write complex λ terms legibly. 

Each UI mode contains help for its specific primitives and modes of operation.
"))



(defun format-warnings (stream)
  (format stream "~&
Warnings!
        * If you enter a term which recurses infinitely, this UI will not catch 
          it, and you'll have to stop the process. (C-c C-c in Emacs)
        * Do not expect bound variables to keep their names, though using single
          letter variables everywhere should give the best results.~%~%"))



(defun initial-help-transformation-viewer (stream)
  (format stream "
transformation viewer
---------------------

Input a λ term using the base primitives, and see the different representations
that are defined for this term.

The primitives, which you can use as keywords, are the following.

")
  (print-environment (basic-combinators) :stream stream)
  (print-environment (recursing-combinators) :stream stream)
  (format stream "
If you input an open λ term, that is one with free variables, which are not in
the basic-encodings, your term will be closed by surrounding it with 
λ-abstractions of your free variables, printing a warning. Note that in this
case, the names of these free variables are not guaranteed to stay the same.

If you write :eval before your term, then your term is first reduced and the 
representations of its reduction are shown."))



(defun initial-help-base (stream)
  (format stream "
base repl
---------

In the base repl you can input λ terms.

The primitives, which you can use as keywords, are the following.

")
  (print-environment (basic-combinators) :stream stream)
  (print-environment (recursing-combinators) :stream stream))


(defun initial-help-church (stream)
  (format stream "
church repl
-----------

In the base repl you can input λ terms, possibly with integers in the place of 
a ^variable. The number will be internally resolved to the corresponding church
numeral. 

Church numerals are defined with the help of repeated application, which is
defined recursively as follows.

(apply-n-times f x     0 ) = 'x
(apply-n-times f x (1+ n)) = \`(f ,(apply-n-times f x n))

The church numeral representing n is then:

(church-numeral n) = \`(λ f (λ x ,(apply-n-times f x n))) 

The primitives, which you can use as keywords, are given by the base 
encodings: ")
  (print-environment (basic-combinators) :stream stream)
  (print-environment (recursing-combinators) :stream stream)
  (format stream "~%~%and the ones given by the following encodings for doing 
church arithmetic:~%~%")
  (print-environment (church-arithmetic) :stream stream))
