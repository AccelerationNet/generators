(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :generators.system)
    (defpackage :generators.system
      (:use :common-lisp :asdf))))

(in-package :generators.system)

(defsystem :generators
  :description "A common lisp package providing python style generators based
                on delimited continuations"
  :author "<programmers@acceleration.net>"
  :licence "BSD"
  :version "0.1"
  :components ((:file "packages")
               (:file "generators"))
  :depends-on (:cl-cont :alexandria :collectors))


(defsystem :generators-test
  :description "Tests for the generators common lisp library"
  :licence "BSD"
  :version "0.1"
  :serial t
  :components ((:module :test
                        :serial t
                        :components ((:file "generators"))))
  :depends-on (:generators :lisp-unit))

(defmethod asdf:perform ((o test-op) (c (eql (find-system :generators))))
  (asdf:load-system :generators-test)
  (funcall (intern "RUNTESTS" :generators-test)))

