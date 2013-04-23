(common-lisp:defpackage :generators-test
  (:shadowing-import-from #:generators #:next)
  (:use :generators :cl :iterate))

(common-lisp:in-package :generators-test)

(cl:with-package-iterator (sym '(:generators) :internal :external)
  (iter (multiple-value-bind (more? symbol accessibility pkg) (sym)
	  (declare (ignore accessibility))
	  (when (eql (find-package :generators)
		     pkg)
	    (ignore-errors
	      (unintern symbol :generators-test)
	      (import symbol :generators-test)))
	  (while more?))))

(defmacro deftest (name (&rest args) &body body)
  (iter (for tag in args)
	(setf (get tag :tests)
	      (union (arnesi:ensure-list (get tag :tests))
		     (list name))))
  `(lisp-unit:define-test ,name
     ,@body))

(defun runtests (&key suites tests (use-debugger T))
  (let* ((*package* (find-package :generators-test))
	 (lisp-unit::*use-debugger* use-debugger)
         (lisp-unit:*print-failures* t)
         (lisp-unit:*print-errors* t)
	 (tests (or (append (arnesi:ensure-list tests)
                            (iter (for suite in (arnesi:ensure-list suites))
			      (appending (get suite :tests))))
                    :all))
	 (out (with-output-to-string (*standard-output*)
                (lisp-unit:run-tests tests))))
    (format *standard-output*
     "~&~% ** TEST RESULTS: Generators ** ~%-----------~%~A~%------ END TEST RESULTS ------~%"
     out)))