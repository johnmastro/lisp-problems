;; Problem 10 ------------------------------------------------------------------

;; Use the result of problem P09 to implement the so-called run-length encoding
;; data compression method. Consecutive duplicates of elements are encoded as
;; lists (N E) where N is the number of duplicates of the element E.

;; Note: using the function from problem 9 is an inefficient way to go about
;; this, and I almost solved the problem directly instead. But, the direct
;; approach is the subject of problem 13 so I'll follow the instructions.

(in-package :lisp-problems)

(defun encode/map (list)
  (mapcar #'(lambda (group)
              (list (length group)
                    (first group)))
          (pack-by/iter list)))

(defun encode/iter (list)
  (let (result)
    (dolist (group (pack-by/iter list))
      (push (list (length group) (first group))
            result))
    (nreverse result)))

(defun encode/loop (list)
  (loop with groups = (pack-by/iter list)
        for group in groups
        collect (list (length group) (first group))))

(defun encode/recur (list)
  (labels ((recur (list acc)
             (if (endp list)
                 (nreverse acc)
                 (let ((group (first list)))
                   (recur (rest list)
                          (cons (list (length group) (first group))
                                acc))))))
    (recur (pack-by/iter list) nil)))

(deftest test-encode ()
  (check-each encode (encode/map encode/iter encode/loop encode/recur)
    (equal (encode nil) nil)
    (equal (encode '(a)) '((1 a)))
    (equal (encode '(a a)) '((2 a)))
    (equal (encode '(a b)) '((1 a) (1 b)))
    (equal (encode '(a a a a b c c a a d e e e e))
           '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)))))
