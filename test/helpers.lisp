;;;;test/helpers.lisp

(in-package :lambda.test)

(defun set-equal (list-1 list-2)
  (and (null (set-difference list-1 list-2))
       (null (set-difference list-2 list-1))))
