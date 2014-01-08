;; Problem 6 -------------------------------------------------------------------

;; Find out whether a list is a palindrome

(in-package :lisp-problems)

(defun palindromep (list)
  (equal list (reverse list)))

(deftest test-palindromep ()
  (check-each palindromep (palindromep)
    (equal (palindromep nil) t)
    (equal (palindromep '(1 2 3)) nil)
    (equal (palindromep '(1 2 3 2 1)) t)))
