;; Problem 4 -------------------------------------------------------------------

;; Find the number of elements of a list.

;; Tail recursive
(defun count-items/rec (list)
  (labels ((cnt (list n)
             (if (null list)
                 n
                 (cnt (cdr list) (1+ n)))))
    (cnt list 0)))

;; Not tail recursive
(defun count-items/rec2 (list)
  (if (null list)
      0
      (1+ (count-items/rec (cdr list)))))

(defun count-items/iter (list)
  (loop for item in list
        count 1))
