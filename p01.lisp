;; Problem 1 -------------------------------------------------------------------

;; Find the last box of a list.

(defun last/rec (list)
  (if (null (cdr list))
      list
      (last/rec (cdr list))))

(defun last/iter (list)
  (loop for sublist on list
        when (null (cdr sublist)) return sublist))
