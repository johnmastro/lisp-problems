;; Problem 9 -------------------------------------------------------------------

;; Pack consecutive duplicates of list elements into sublists.
;; (This is more commonly known as `group-by` or `partition-by`).

(in-package :lisp-problems)

(defun pack-by/iter (list &key (test #'eql))
  (flet ((iter (list)
           (let ((result (list (list (car list)))))
             (dolist (item (cdr list))
               (if (funcall test item (caar result))
                   (push item (car result))
                   (push (list item) result)))
             (nreverse result))))
    (cond ((null list) list)
          ((null (cdr list)) (list list))
          (t (iter list)))))

(defun pack-by/rec (list &key (test #'eql))
  (labels ((rec (item group list)
             (cond ((null list) (list (cons item group)))
                   ((funcall test item (car list))
                    (rec item (cons item group) (cdr list)))
                   (t (cons (cons item group)
                            (rec (car list) nil (cdr list)))))))
    (cond ((null list) list)
          ((null (cdr list)) (list list))
          (t (rec (car list) nil (cdr list))))))

(defun pack-by/loop (list &key (test #'eql))
  (flet ((run-loop (list)
           (loop with result = (list (list (car list)))
                 for item in (cdr list) do
                 (if (funcall test item (caar result))
                     (push item (car result))
                     (push (list item) result))
                 finally (return (nreverse result)))))
    (cond ((null list) list)
          ((null (cdr list)) (list list))
          (t (run-loop list)))))

(deftest test-pack-by ()
  (check-each pack-by (pack-by/iter pack-by/rec pack-by/loop)
    (equal (pack-by nil) nil)
    (equal (pack-by '(a)) '((a)))
    (equal (pack-by '(a a)) '((a a)))
    (equal (pack-by '(a b)) '((a) (b)))
    (equal (pack-by '(a a a a b c c a a d e e e e))
           '((a a a a) (b) (c c) (a a) (d) (e e e e)))))

