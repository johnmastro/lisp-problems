;; Problem 1 -------------------------------------------------------------------

;; Find the last box of a list.

(in-package :lisp-problems)

(defun last/rec (list)
  (if (null (cdr list))
      list
      (last/rec (cdr list))))

(defun last/iter (list)
  (loop for sublist on list
        when (null (cdr sublist)) return sublist))

(deftest test-last ()
  (check-each last* (last/rec last/iter)
    (equal (last* nil) nil)
    (equal (last* '(1)) '(1))
    (equal (last* '(1 2)) '(2))))
