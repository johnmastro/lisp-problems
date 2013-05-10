;; Problem 7 -------------------------------------------------------------------

;; Flatten a nested list structure

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
  (check-each (flatten/rec flatten/iter)
    (equal (funcall it nil) nil)
    (equal (funcall it '(a)) '(a))
    (equal (funcall it '(a (b c) (d))) '(a b c d))
    (equal (funcall it '(a (b (c) ((d) ((e)) f)))) '(a b c d e f))))
