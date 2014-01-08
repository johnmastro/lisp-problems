;;;; lisp-problems.asd

(asdf:defsystem #:lisp-problems
  :serial t
  :description "My solutions to the \"Ninety-Nine Lisp Problems\"."
  :author "John Mastro <john.b.mastro@gmail.com>"
  :license "Specify license here"
  :components ((:file "packages")
               (:file "test")
               (:file "p01")
               (:file "p02")
               (:file "p03")
               (:file "p04")
               (:file "p05")
               (:file "p06")
               (:file "p07")
               (:file "p08")
               (:file "p09")
               (:file "p10")
               (:file "p11")
               (:file "p12")))

