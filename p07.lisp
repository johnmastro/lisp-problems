;; Problem 7 -------------------------------------------------------------------

;; Flatten a nested list structure

(in-package :lisp-problems)

(defun flatten/rec (list)
  (cond ((null list) nil)
        ((listp (car list))
         (append (flatten/rec (car list)) (flatten/rec (cdr list))))
        (t (cons (car list) (flatten/rec (cdr list))))))

(defun flatten/iter (list)
  (let (result)
    (labels ((walk (list)
               (dolist (node list)
                 (if (listp node)
                     (walk node)
                     (push node result)))))
      (walk list)
      (nreverse result))))

(deftest test-flatten ()
  (check-each flatten (flatten/rec flatten/iter)
    (equal (flatten nil) nil)
    (equal (flatten '(a)) '(a))
    (equal (flatten '(a (b c) (d))) '(a b c d))
    (equal (flatten '(a (b (c) ((d) ((e)) f)))) '(a b c d e f))))
