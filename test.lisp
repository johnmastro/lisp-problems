;; This is the unit-testing code from chapter 9 of Practical Common Lisp with
;; one additional macro added: `check-each`. It allows a group of tests to be
;; specified once for multiple implementations of a function. It's anaphoric
;; and hacky, but I've found it convenient for these problems since I've been
;; solving each more than once but don't want to have to specify the tests
;; repeatedly.
;;
;; See http://www.gigamonkeys.com/book/ for the original code and more.

(in-package :lisp-problems.test)

(defvar *test-name* nil)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro combine-results (&body forms)
  (let ((result (gensym)))
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro check-each (name (&rest functions) &body forms)
  (let ((args (gensym)))
    `(combine-results
       ,@(loop for fn in functions collect
               `(flet ((,name (&rest ,args)
                         (apply #',fn ,args)))
                  (let ((*test-name* (append *test-name* (list ',fn))))
                    (check ,@forms)))))))

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

