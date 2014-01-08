;; Problem 5 -------------------------------------------------------------------

;; Reverse a list.

(in-package :lisp-problems)

(defun reverse/rec (list)
  (labels ((rev (list acc)
             (if (null list)
                 acc
                 (rev (cdr list) (cons (car list) acc)))))
    (rev list nil)))

(defun reverse/iter (list)
  (loop with result = nil
        for item in list
        do (push item result)
        finally (return result)))
