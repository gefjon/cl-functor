(uiop:define-package #:functor/fmap
  (:mix #:cl #:iterate)
  (:import-from #:closer-mop
                #:class-slots
                #:slot-definition-name
                #:slot-definition-type
                #:slot-value-using-class
                #:compute-applicable-methods-using-classes)
  (:import-from #:alexandria
                #:curry #:with-gensyms #:if-let)
  (:export
   #:fmap-all-slots-mixin
   #:fmap
   #:fmap-slots
   #:functor-specific-slots))
(in-package #:functor/fmap)

(defun class-for-type (type)
  (etypecase type
    (symbol (find-class type nil))
    (list (find-class (first type) nil))))

(defgeneric fmap (func obj)
  (:documentation "Map FUNC across the functor OBJ."))

(defun class-functor-p (class)
  (multiple-value-bind (methods success-p)
      (compute-applicable-methods-using-classes #'fmap (list (find-class t) class))
    (when success-p methods)))

(defun slot-functor-p (slot)
  (class-functor-p (class-for-type (slot-definition-type slot))))

(defun fmap-slots-by-pred (func instance recurse-on-slot-p ignore-slot-p)
  "Map FUNC across the `standard-object' INSTANCE by applying (FMAP FUNC) to any slot for which RECURSE-ON-SLOT-P returns non-nil, or FUNC to any slot for which IGNORE-SLOT-P returns nil.

Returns a new instance of the same class as INSTANCE. RECURSE-ON-SLOT-P and IGNORE-SLOT-P will be called with
one argument, the `effective-slot-definition's of that class. RECURSE-ON-SLOT-P takes precedence over
IGNORE-SLOT-P. Any ignored slot will have its value copied from INSTANCE."
  (iter
    (with class = (class-of instance))
    (with new = (allocate-instance class))
    (with slots = (class-slots class))
    (with recurse = (curry #'fmap func))
    (for slot in slots)
    (for old-val = (slot-value-using-class class instance slot))
    (setf (slot-value-using-class class new slot)
          (cond ((funcall recurse-on-slot-p slot) (funcall recurse old-val))
                ((funcall ignore-slot-p slot) old-val)
                (:otherwise (funcall func old-val))))
    (finally (return (reinitialize-instance new)))))

(defmethod fmap (func (obj sequence))
  "Use `map' to produce a `sequence' of the same type as OBJ."
  (map (type-of obj) func obj))

(defclass fmap-all-slots-mixin ()
  ()
  (:documentation "A mixin for `standard-class'es which causes `fmap' to recurse into each of its slots which has a declared type which is a functor, and to map across each other slot."))

(defmethod fmap (func (obj fmap-all-slots-mixin))
  "Apply FUNC to each slot of OBJ, returning a new instance of the same class."
  (fmap-slots-by-pred func obj #'slot-functor-p (constantly nil)))

(defun fmap-slots (func instance functor-slots mapped-slots)
  "Map FUNC across INSTANCE by applying (FMAP FUNC) to each of the slots named in FUNCTOR-SLOTS and applying FUNC to each of the slots named in MAPPED-SLOTS, preserving the values of all slots not named."
  (labels ((slot-definition-has-name-p (slot-definition name)
             (eq (slot-definition-name slot-definition) name))
           (functor-slot-p (slot)
             (member slot functor-slots :test #'slot-definition-has-name-p))
           (map-slot-p (slot)
             (member slot mapped-slots :test #'slot-definition-has-name-p)))
    (fmap-slots-by-pred func instance #'functor-slot-p (complement #'map-slot-p))))

(defmacro functor-specific-slots (class-name &key functor-slots mapped-slots)
  "Define an `fmap' method for CLASS-NAME which recurses into the FUNCTOR-SLOTS, maps across the MAPPED-SLOTS and leaves other slots unaltered."
  (with-gensyms (func obj)
    `(defmethod fmap (,func (,obj ,class-name))
       (fmap-slots ,func ,obj ',functor-slots ',mapped-slots))))

