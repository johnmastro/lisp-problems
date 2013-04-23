;; Problem 6 -------------------------------------------------------------------

;; Find out whether a list is a palindrome

(defun palindromep (list)
  (equal list (reverse list)))
