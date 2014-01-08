;; Problem 6 -------------------------------------------------------------------

;; Find out whether a list is a palindrome

(in-package :lisp-problems)

(defun palindromep (list)
  (equal list (reverse list)))
