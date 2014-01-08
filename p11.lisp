;; Problem 11 ------------------------------------------------------------------

;; Modify the result of problem P10 in such a way that if an element has no
;; duplicates it is simply copied into the result list. Only elements with
;; duplicates are transferred as (N E) lists.

(in-package :lisp-problems)

(defun encode2/map (list)
  (mapcar #'(lambda (group)
              (let ((length (length group))
                    (item (first group)))
                (if (> length 1)
                    (list length item)
                    item)))
          (pack-by/iter list)))

(defun encode2/iter (list)
  (let (result)
    (dolist (group (pack-by/iter list))
      (let ((length (length group))
            (item (first group)))
        (push (if (> length 1) (list length item) item)
              result)))
    (nreverse result)))

(defun encode2/loop (list)
  (loop for group in (pack-by/iter list)
        for length = (length group) for item = (first group)
        when (> length 1) collect (list length item)
        else collect  item))

(defun encode2/recur (list)
  (labels ((recur (list acc)
             (if (endp list)
                 (nreverse acc)
                 (let* ((group (first list))
                        (length (length group))
                        (item (first group))
                        (it (if (> length 1) (list length item) item)))
                   (recur (rest list) (cons it acc))))))
    (recur (pack-by/iter list) nil)))

(deftest test-encode2 ()
  (check-each encode2 (encode2/map
                       encode2/iter
                       encode2/loop
                       encode2/recur)
    (equal (encode2 nil) nil)
    (equal (encode2 '(a)) '(a))
    (equal (encode2 '(a a)) '((2 a)))
    (equal (encode2 '(a b)) '(a b))
    (equal (encode2 '(a a a a b c c a a d e e e e))
           '((4 a) b (2 c) (2 a) d (4 e)))))
