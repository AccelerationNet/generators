(in-package :generators-test)

(deftest basic-generator (generator-tests)
  (let ((gen (make-generator ()
		(iter (for i from 0 to 10)
		      (yield i)))))
    (lisp-unit:assert-equal
     (iter (for i from 0 to 10) (collect i))
     (iter (for i in-generator gen) (collect i)))
    (reset gen)
    (lisp-unit:assert-equal
     (iter (for i from 0 to 10) (collect i))
     (iter (for i in-generator gen) (collect i)))))

(deftest notrec-generator (generator-tests)
  (let* ((exp '(1 2 3 4 5 1 2 3 4 5))
	 (g1 (make-generator ()
	       (dotimes (i 5) (yield (+ 1 i)))))
	 (g2 (make-generator ()
	       (dotimes (i 2)
		 (reset g1)
		 (dolist (v (force g1)) (yield v))))))
    (lisp-unit:assert-equal exp (force g2))))

(deftest rec-generator (generator-tests)
  (let* ((exp '(1 2 3 4 5 1 2 3 4 5))
	 (g1 (make-generator ()
	       (dotimes (i 5) (yield (+ 1 i)))))
	 (g2 (make-generator ()
	       (dotimes (i 2) (reset g1) (yielding g1)))))
    (lisp-unit:assert-equal exp (force g2)) (reset g2)
    (lisp-unit:assert-equal exp (iter (for i in-generator g2) (collect i)))
    ))

(deftest rec2-generator (generator-tests)
  (let* ((expected-results
	  (iter top
		(for i from 1 to 2)
		(iter (for j from 1 to 5)
		      (in top (collect j)))))
	 (g1 (make-generator () (iter (for i from 1 to 5) (yield i))))
	 (g2 (make-generator ()
	       (iter (for i from 1 to 2)
		     (reset g1)
		     (yielding g1)))))
    (lisp-unit:assert-equal
     expected-results (force g2))
    (reset g2)
    (lisp-unit:assert-equal
     expected-results (iter (for i in-generator g2) (collect i)))
    ))

(deftest lisp-tree-generators (generator-tests)
  (let* ((tree '(1
		 (2 3)
		 (4 5)
		 (6
		  (7 8)
		  (9 10)
		  (11 (12 13)))))
	 (exp (iter (for i from 1 to 13) (collect i)))
	 (exp2 '(1 (2 3) 2 3 (4 5) 4 5
		 (6 (7 8) (9 10) (11 (12 13)))
		 6 (7 8) 7 8 (9 10) 9 10
		 (11 (12 13)) 11 (12 13) 12 13
		 ))
	 (leaves (generate-lisp-tree-nodes tree t))
	 (nodes (generate-lisp-tree-nodes tree)))
    (lisp-unit:assert-equal
     exp (force leaves))
    (reset leaves)
    
    (lisp-unit:assert-equal
     exp
     (iter (for i in-generator leaves) (collect i)))
    
    (lisp-unit:assert-equal
     exp
     (iter (for i a-leaf-of-lisp-tree tree) (collect i)))
    
    (lisp-unit:assert-equal
     exp2
     (iter (for i in-generator nodes) (collect i)))

    (lisp-unit:assert-equal
     exp2
     (iter (for i a-node-of-lisp-tree tree) (collect i)))
    ))

