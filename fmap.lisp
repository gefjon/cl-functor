(uiop:define-package #:functor/fmap
  (:mix #:cl #:iterate)
  (:import-from #:closer-mop
                #:class-slots #:slot-definition-name #:slot-value-using-class)
  (:import-from #:alexandria
                #:curry #:with-gensyms)
  (:export
   #:fmap-all-slots-mixin
   #:fmap
   #:fmap-slots
   #:functor-specific-slots))
(in-package #:functor/fmap)

(defgeneric fmap (func obj)
  (:documentation "Map FUNC across the functor OBJ."))

(defun fmap-slots-by-pred (func instance recurse-on-slot-p)
  "Map FUNC across INSTANCE by applying FUNC to any slot for which RECURSE-ON-SLOT-P returns non-nil.

Returns a new instance of the same class as INSTANCE. RECURSE-ON-SLOT-P will be called with one argument, the
`effective-slot-definition's of that class. Any slot definition for which it returns `nil' will be inserted
into the new instance as-is, and all others will have the value of applying FUNC to that slot-value inserted."
  (iter
    (with class = (class-of instance))
    (with new = (allocate-instance class))
    (with slots = (class-slots class))
    (with recurse = (curry #'fmap func))
    (for slot in slots)
    (for old-val = (slot-value-using-class class instance slot))
    (setf (slot-value-using-class class new slot)
          (if (funcall recurse-on-slot-p slot)
              (funcall recurse old-val)
              old-val))
    (finally (return (reinitialize-instance new)))))

(defmethod fmap (func obj)
    "Base case for recursive invocations: invoke FUNC on OBJ.

Note that the existence of this method means that `fmap' can be
applied to any object, so unlike in e.g. Haskell, (fmap #'- 5) is
valid and returns -5."
    (funcall func obj))

(defmethod fmap (func (obj sequence))
  "Use `map' to produce a `sequence' of the same type as OBJ."
  (map (type-of obj) (curry #'fmap func) obj))

(defclass fmap-all-slots-mixin ()
  ()
  (:documentation "A mixin for `standard-class'es which causes `fmap' to map across each of its bound slots."))

(defmethod fmap (func (obj fmap-all-slots-mixin))
  "Apply FUNC to each slot of OBJ, returning a new instance of the same class."
  (fmap-slots-by-pred func obj (constantly t)))

(defun fmap-slots (func instance &rest slots-to-recurse)
  "Map FUNC across INSTANCE by applying FUNC to each of the slots whose names are in SLOTS-TO-RECURSE, preserving the values of all slots not named."
  (labels ((slot-definition-has-name-p (slot-definition name)
             (eq (slot-definition-name slot-definition) name))
           (recurse-on-slot-p (slot)
             (member slot slots-to-recurse :test #'slot-definition-has-name-p)))
    (fmap-slots-by-pred func instance #'recurse-on-slot-p)))

(defmacro functor-specific-slots (class-name &rest slot-names)
  "Define an `fmap' method for CLASS-NAME which maps across the SLOT-NAMES and leaves other slots unaltered."
  (with-gensyms (func obj)
    `(defmethod fmap (,func (,obj ,class-name))
       (apply #'fmap-slots ,func ,obj ',slot-names))))

