;; Problem 12 ------------------------------------------------------------------

;; Given a run-length code list generated as specified in problem P11 construct
;; its uncompressed version.

(in-package :lisp-problems)

(defun unpack (obj)
  (if (listp obj)
      (make-list (first obj) :initial-element (second obj))
      (list obj)))

(defun decode/map (list)
  (mapcan #'unpack list))

(defun decode/iter (list)
  (let (result)
    (dolist (item list)
      (if (listp item)
          (dotimes (_ (first item))
            (push (second item) result))
          (push item result)))
    (nreverse result)))

(defun decode/loop (list)
  (loop for item in list nconc (unpack item)))

(defun decode/fold (list)
  (reduce #'(lambda (result item)
              (nconc result (unpack item)))
          list
          :initial-value nil))

(deftest test-decode ()
  (check-each decode (decode/map decode/iter decode/loop decode/fold)
    (equal (decode nil) nil)
    (equal (decode '(a)) '(a))
    (equal (decode '((2 a))) '(a a))
    (equal (decode '((4 a) b (2 c) (2 a) d (4 e)))
           '(a a a a b c c a a d e e e e))))
