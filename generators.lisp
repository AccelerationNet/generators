(in-package :generators)

(define-condition stop-iteration (error)
  ((generator :accessor generator :initarg :generator  :initform nil)
   (handled? :accessor handled? :initarg :handled? :initform nil))
  (:report (lambda (c s)
             (format s "The generator has stopped:~S <~S>"
		     (generator c)
		     (handled? c)))))

(defun %stop-iteration (generator)
  (if (use-final-value? generator)
      (final-value generator)
      (error (make-condition
              (or (final-exception generator)
                  'stop-iteration)
              :generator generator))))

(defclass generator ()
  ((continuation
    :accessor continuation :initform nil :initarg :continuation
    :documentation "The current continuation of the generator")
   (use-final-value?
    :accessor use-final-value? :initform nil :initarg :use-final-value?
    :documentation "Should this generator, generate a sigil value when complete
     if nil will raise the final-exception (defaults 'stop-iteration)")
   (final-value
    :accessor final-value :initform nil :initarg :final-value
    :documentation "The value to generate if we are using a final-value
    instead of raising exceptions")
   (final-exception
    :accessor final-exception :initform 'stop-iteration :initarg :final-exception
    :documentation "An exception to raise when we are done iterating (if not using final-value)")
   (finished?
    :accessor finished? :initform nil :initarg :finished?
    :documentation "Has this generator generated all values?" )
   (original-continuation
    :accessor original-continuation :initform nil :initarg :original-continuation
    :documentation "The starting point of this generator")
   (name :accessor name :initarg :name :initform nil))
  (:documentation "A class that stores all the needed structure for a generator"))

(defgeneric next ( generator &rest args)
  (:documentation "Returns the next value of this generator")
  (:method ((gen generator) &rest args)
    (if (finished? gen)
        (%stop-iteration gen)
        (apply (continuation gen) args))))

(defgeneric reset (generator)
  (:documentation "Restarts the generator from its original continuation")
  (:method ((gen generator))
    (setf (continuation gen) (original-continuation gen)
          (finished? gen) nil)))

(iterate:defmacro-driver (FOR node in-generator gen)
  (let ((kwd (if generate 'generate 'for)))
    (alexandria:with-unique-names (g end?)
      `(let ((,g ,gen))
        (declare (ignorable ,g))
        (,kwd ((,node) ,end?) next (%mv-gen ,gen))
        (until ,end?)))))

(defgeneric force (generator)
  (:documentation "Forces the generator to produce a list of its output")
  (:method ((g generator))
    (iter (for n in-generator g) (collect n))))

(defun %mv-gen (g)
  "Generates a value from the generator, if the generator throws
   stop iteration returns (values nil T),
   turns off call/cc so that this works"
  (cl-cont:without-call/cc
    (handler-bind
        ((error
           (lambda (c)
             (when (typep c (final-exception g))
               (return-from %mv-gen (list nil T))))))
      (list (multiple-value-list (next g)) nil))))

(defun only-one? (list)
  (and (listp list) (null (cdr list)) (car list)))

(defmacro make-generator ((&key (final-value nil final-value-p)
                           (final-exception '(quote stop-iteration))
                           name)
			  &body body)
  "returns a function that when called yields the next values.  Inside of
   body, you can yield any number of values that you wish by calling
   (yield &rest args) which is scoped to the body
  "
  (alexandria:with-unique-names (gen)
    `(let ((,gen (make-instance 'generator
                  :use-final-value? ,final-value-p
                  :final-value ,final-value
                  :final-exception ,final-exception
                  :name ,name)))
      (cl-cont:with-call/cc
        ;; set and return the orignal
        (cl-cont:let/cc k
          (unless (original-continuation ,gen)
            (setf (original-continuation ,gen) k)
            (reset ,gen)
            ,gen))
        (labels ((yield (&rest args)
                   (cl-cont:let/cc k
                     (setf (continuation ,gen) k)
                     (apply #'values args)))
                 (yielding (&rest other-gens)
                   ;; NB: !! iterate fails here (we dont always yield everything), use loop !!
                   (loop for g in (alexandria:ensure-list other-gens)
                         do (loop for (vals end?) = (%mv-gen g)
                                  until end?
                                  do (apply #'yield vals)))))
          ,@body
          (setf (finished? ,gen) T (continuation ,gen) nil)
          (loop (next ,gen)))))))

(defun generate-lisp-tree-nodes (trees &optional leaves-only?)
  "Do a depth first traversal of some set of trees yielding every node "
  (make-generator ()
    (iter (for n in (alexandria:ensure-list trees))
      (etypecase n
        (atom (yield n))
        (list
         (unless leaves-only? (yield n))
         (yielding (generate-lisp-tree-nodes n leaves-only?)))))))

(iterate:defmacro-driver (FOR node a-node-of-lisp-tree tree)
  (let* ((kwd (if generate 'generate 'for)))
    (alexandria:with-unique-names (gen end?)
      `(progn
        (with ,gen = (generate-lisp-tree-nodes ,tree))
        (,kwd ((,node) ,end?) next (%mv-gen ,gen))
        (until ,end?)))))

(iterate:defmacro-driver (FOR node a-leaf-of-lisp-tree tree)
  (let* ((kwd (if generate 'generate 'for)))
    (alexandria:with-unique-names (gen end?)
      `(progn
        (with ,gen = (generate-lisp-tree-nodes ,tree T))
        (,kwd ((,node) ,end?) next (%mv-gen ,gen))
        (until ,end?)))))
