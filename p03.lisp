;; Problem 3 -------------------------------------------------------------------

;; Find the K'th element of a list.

(in-package :lisp-problems)

(defun element-at/rec (list n)
  (cond ((< n 1) (error "Index error: elements start at 1"))
        ((= n 1) (car list))
        (t (element-at/rec (cdr list) (1- n)))))

(defun element-at/iter (list n)
  (loop for item in list
        for i downfrom n
        when (= i 1) return item))

(deftest test-element-at ()
  (let ((numbers (loop for i from 0 below 10 collect i)))
    (check-each elt-at (element-at/rec element-at/iter)
      (eql (elt-at nil 3) nil)
      (eql (elt-at numbers 1) 0)
      (eql (elt-at numbers 6) 5)
      (eql (elt-at numbers 20) nil))))
