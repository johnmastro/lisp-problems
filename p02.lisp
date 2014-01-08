;; Problem 2 -------------------------------------------------------------------

;; Find the last but one box of a list.

;; Note that this is different from CL:BUTLAST

(in-package :lisp-problems)

(defun butlast/rec (list)
  (if (null (cddr list))
      list
      (butlast/rec (cdr list))))

(defun butlast/iter (list)
  (loop for sublist on list
        when (null (cddr sublist)) return sublist))

;; Extra credit; find the last `n` elements (like CL:LAST)
(defun last/n (list &optional (n 1))
  (loop for sublist on list
        when (null (nthcdr n sublist)) return sublist))

(deftest test-butlast ()
  (check-each butlast* (butlast/rec butlast/iter)
    (equal (butlast nil) nil)
    (equal (butlast '(1)) nil)
    (equal (butlast '(1 2)) '(1))))
