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
