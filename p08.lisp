;; Problem 8 -------------------------------------------------------------------

;; Eliminate consecutive duplicates of list elements

(in-package :lisp-problems)

(defun compress/rec (list &key (test #'eql))
  (labels ((rec (item list)
             (cond ((null list) (list item))
                   ((funcall test item (car list)) (rec item (cdr list)))
                   (t (cons item (rec (car list) (cdr list)))))))
    (if (null (cdr list))
        list
        (rec (car list) (cdr list)))))

(defun compress/iter (list &key (test #'eql))
  (if (null (cdr list))
      list
      (let ((result nil)
            (item (car list)))
        (dolist (other (cdr list))
          (unless (funcall test item other)
            (push item result)
            (setf item other)))
        (push item result)
        (nreverse result))))

(defun compress/fold (list &key (test #'eql))
  (reduce #'(lambda (item result)
              (if (and result (funcall test item (car result)))
                  result
                  (cons item result)))
          list
          :from-end t
          :initial-value nil))

;; A simplified iterative solution without a :test parameter. Uses
;; `gensym` to avoid treating the beginning/end of the list as a
;; special case but could blow up with a test parameter of e.g.
;; #'(lambda (x y) (equal (type-of x) (type-of y))).
;;
;; Using `gensym` like this isn't idiomatic but it does meet the
;; requirements of the problem as stated.
(defun compress/naive-iter (list)
  (let ((result nil)
        (other (gensym)))
    (dolist (item list)
      (unless (eql item other)
        (push item result)
        (setf other item)))
    (nreverse result)))

(deftest test-compress ()
  (check-each compress (compress/rec
                        compress/iter
                        compress/fold
                        compress/naive-iter)
    (equal (compress nil) nil)
    (equal (compress '(a)) '(a))
    (equal (compress '(a a)) '(a))
    (equal (compress '(a b)) '(a b))
    (equal (compress '(a a b b)) '(a b))
    (equal (compress '(a a a a b c c a a d e e e e))
           '(a b c a d e))))
