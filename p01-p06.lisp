;; Problem 1 -------------------------------------------------------------------

;; Find the last box of a list.

(defun last/rec (list)
  (if (null (cdr list))
      list
      (last/rec (cdr list))))

(defun last/iter (list)
  (loop for sublist on list
        when (null (cdr sublist)) return sublist))

;; Problem 2 -------------------------------------------------------------------

;; Find the last but one box of a list.

(defun butlast/rec (list)
  (if (null (cddr list))
      list
      (butlast/rec (cdr list))))

(defun butlast/iter (list)
  (loop for sublist on list
        when (null (cddr sublist)) return sublist))

;; Extra credit; find the last `n` elements (like CL:LAST)
(defun last/n (list &optional (n 1))
  (loop for sublist on list
        when (null (nthcdr n sublist)) return sublist))

;; Problem 3 -------------------------------------------------------------------

;; Find the K'th element of a list.

(defun element-at/rec (list n)
  (cond ((< n 1) (error "Index error: elements start at 1"))
        ((= n 1) (car list))
        (t (element-at/rec (cdr list) (1- n)))))

(defun element-at/iter (list n)
  (loop for item in list
        for i downfrom n
        when (= i 1) return item))

;; Problem 4 -------------------------------------------------------------------

;; Find the number of elements of a list.

(defun count-items/rec (list)
  (labels ((cnt (list n)
             (if (null list)
                 n
                 (cnt (cdr list) (1+ n)))))
    (cnt list 0)))

(defun count-items/rec (list)
  (if (null list)
      0
      (1+ (count-items/rec (cdr list)))))

(defun count-items/iter (list)
  (loop for item in list
        count 1))

;; Problem 5 -------------------------------------------------------------------

;; Reverse a list.

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

;; Problem 6 -------------------------------------------------------------------

;; Find out whether a list is a palindrome

(defun palindromep (list)
  (equal list (reverse list)))
