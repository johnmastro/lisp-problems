;; Problem 11 ------------------------------------------------------------------

;; Modify the result of problem P10 in such a way that if an element has no
;; duplicates it is simply copied into the result list. Only elements with
;; duplicates are transferred as (N E) lists.

(defun encode-modified/map (list)
  (mapcar #'(lambda (group)
              (let ((length (length group))
                    (item (first group)))
                (if (> length 1)
                    (list length item)
                    item)))
          (pack-by/iter list)))

(defun encode-modified/iter (list)
  (let (result)
    (dolist (group (pack-by/iter list))
      (let ((length (length group))
            (item (first group)))
        (push (if (> length 1) (list length item) item)
              result)))
    (nreverse result)))

(defun encode-modified/loop (list)
  (loop for group in (pack-by/iter list)
        for length = (length group) for item = (first group)
        when (> length 1) collect (list length item)
        else collect  item))

(defun encode-modified/recur (list)
  (labels ((recur (list acc)
             (if (endp list)
                 (nreverse acc)
                 (let* ((group (first list))
                        (length (length group))
                        (item (first group))
                        (it (if (> length 1) (list length item) item)))
                   (recur (rest list) (cons it acc))))))
    (recur (pack-by/iter list) nil)))

(deftest test-encode-modified ()
  (check-each (encode-modified/map
               encode-modified/iter
               encode-modified/loop
               encode-modified/recur)
    (equal (funcall it nil) nil)
    (equal (funcall it '(a)) '(a))
    (equal (funcall it '(a a)) '((2 a)))
    (equal (funcall it '(a b)) '(a b))
    (equal (funcall it '(a a a a b c c a a d e e e e))
           '((4 a) b (2 c) (2 a) d (4 e)))))
