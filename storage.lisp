(in-package #:org.shirakumo.redist)

(defvar *storage*)

(defclass storage () ())

(defgeneric fetch (storage object slot))
(defgeneric store (storage object slot))

(defclass stored-object () ())

(defmethod c2mop:slot-value-using-class :before ((class c2mop:standard-class) (object stored-object) slotd)
  (unless (c2mop:slot-boundp-using-class class object slotd)
    (fetch *storage* object (c2mop:slot-definition-name slotd))))

(defmethod (setf c2mop:slot-value-using-class) :after (value (class c2mop:standard-class) (object stored-object) slotd)
  (store *storage* object (c2mop:slot-definition-name slotd)))

(defmethod fetch ((storage storage) (object stored-object) slot))
(defmethod store ((storage storage) (object stored-object) slot))
