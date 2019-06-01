;;;;compiler.lisp

(in-package :lambda.compiler)

;; Closures of open expressions as a common-lisp function
;;
;; We make an open expression closed, by wrapping it in a closure, a surrounding
;; λ binding for each free variable.
;; The λ-closure of a closed expression is the closed expression itself.

;; A compiler to common-lisp

(defun ^compile (term &key
			(environment NIL)
			(steps 10))
  "Simplifies, closes, and translates the λ TERM to a common-lisp anonymous 
function."
  (let ((closed-simplified-term (full-α-conversion ;; to not bind actual cl vars
				 (^closure
				  (^eval-normal-steps
				   steps 
				   term
				   environment)))))
    (eval (standard->common-lisp closed-simplified-term))))

