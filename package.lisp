(uiop:define-package #:functor/package
  (:nicknames #:functor)
  (:mix #:cl #:iterate)
  (:import-from #:closer-mop
                #:class-slots #:slot-definition-name #:slot-value-using-class)
  (:import-from #:alexandria
                #:curry #:with-gensyms))
(in-package #:functor/package)

(defclass fmap-all-slots-mixin ()
  ())

(defgeneric fmap (func obj)
  (:method (func obj)
    "Base case for recursive invocations: invoke FUNC on OBJ.

Note that the existence of this method means that `fmap' can be
applied to any object, so unlike in e.g. Haskell, (fmap #'- 5) is
valid and returns -5."
    (funcall func obj))
  (:method (func (obj sequence))
    (map (type-of obj) (curry #'fmap func) obj))
  (:method (func (obj fmap-all-slots-mixin))
    (iter
      (with class = (class-of obj))
      (with new = (allocate-instance class))
      (with slots = (class-slots class))
      (with recurse = (curry #'fmap func))
      (for slot in slots)
      (setf (slot-value-using-class class new slot)
            (funcall recurse (slot-value-using-class class obj slot)))
      (finally (return (reinitialize-instance new))))))

(defun fmap-slots (func instance &rest slots-to-recurse)
  (labels ((slot-definition-has-name-p (slot-definition name)
             (eq (slot-definition-name slot-definition) name))
           (recurse-on-slot-p (slot)
             (member slot slots-to-recurse :test #'slot-definition-has-name-p)))
    (iter
      (with class = (class-of instance))
      (with new = (allocate-instance class))
      (with slots = (class-slots class))
      (with recurse = (curry #'fmap func))
      (for slot in slots)
      (for old-val = (slot-value-using-class class instance slot))
      (setf (slot-value-using-class class new slot)
            (if (recurse-on-slot-p slot)
                (funcall recurse old-val)
                old-val))
      (finally (return new)))))

(defmacro functor-specific-slots (class-name &rest slot-names)
  (with-gensyms (func obj)
    `(defmethod fmap (,func (,obj ,class-name))
       (apply #'fmap-slots ,func ,obj ',slot-names))))
