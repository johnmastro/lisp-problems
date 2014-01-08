(defpackage lisp-problems.test
  (:use :common-lisp)
  (:export
   :deftest
   :check
   :check-each
   :combine-results))

(defpackage lisp-problems
  (:use :common-lisp)
  (:import-from :lisp-problems.test :deftest :check-each))

