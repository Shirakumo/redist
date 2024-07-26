(in-package #:org.shirakumo.redist)

(defvar *dists* (make-hash-table :test 'equalp))
(defvar *projects* (make-hash-table :test 'equalp))
(defvar *storage*)

(defclass storage () ())

(defgeneric retrieve (storage object slot))
(defgeneric store (storage object slot))

(defclass stored-object ()
  ((id :initarg :id :writer (setf id))))

(defmethod stored-p ((object stored-object))
  (slot-boundp object 'id))

(defmethod id ((object stored-object))
  (unless (slot-boundp object 'id)
    (store *storage* object T))
  (slot-value object 'id))

(defmethod c2mop:slot-value-using-class :before ((class c2mop:standard-class) (object stored-object) slotd)
  (unless (c2mop:slot-boundp-using-class class object slotd)
    (retrieve *storage* object (c2mop:slot-definition-name slotd))))

(defmethod (setf c2mop:slot-value-using-class) :before (value (class c2mop:standard-class) (object stored-object) slotd)
  (when (c2mop:slot-boundp-using-class class object slotd)
    (store *storage* object (c2mop:slot-definition-name slotd))))

(defmethod retrieve ((storage storage) (object stored-object) slot))
(defmethod store ((storage storage) (object stored-object) slot))

(defmethod retrieve ((storage storage) (all (eql T)) (all2 (eql T)))
  (retrieve storage 'project T)
  (retrieve storage 'dist T))

(defmethod retrieve ((storage (eql T)) object slot)
  (retrieve *storage* object slot))

(defmethod store ((storage (eql T)) object slot)
  (store *storage* object slot))

(defmethod store :before ((storage storage) (object stored-object) slot)
  (unless (or (stored-p object) (eql slot T))
    (store storage object T)))

(defmethod store :around ((storage storage) (object stored-object) slot)
  (when (slot-boundp object slot) ; If we haven't retrieved the slot, we don't need to store it either.
    (call-next-method)))

(defmethod store ((storage storage) (all (eql T)) (all2 (eql T)))
  (store storage 'project T)
  (store storage 'dist T))

(defmethod store ((storage storage) (type (eql 'project)) (all (eql T)))
  (loop for object being the hash-values of *projects*
        do (store storage object T)))

(defmethod store ((storage storage) (type (eql 'dist)) (all (eql T)))
  (loop for object being the hash-values of *dists*
        do (store storage object T)))

(defun clear ()
  (clrhash *dists*)
  (clrhash *projects*))

(defmethod dist ((name symbol))
  (dist (string name)))

(defmethod dist ((name string))
  (or (gethash name *dists*)
      (retrieve *storage* 'dist name)))

(defmethod (setf dist) (dist (name symbol))
  (setf (dist (string name)) dist))

(defmethod (setf dist) (dist (name string))
  (setf (gethash name *dists*) dist))

(defun list-dists ()
  (when (= 0 (hash-table-count *dists*))
    (retrieve *storage* 'dist T))
  (sort (alexandria:hash-table-values *dists*) #'string< :key #'name))

(defmethod project ((name string))
  (or (gethash name *projects*)
      (retrieve *storage* 'project name)))

(defmethod (setf project) (project (name string))
  (setf (gethash name *projects*) project))

(defun list-projects ()
  (when (= 0 (hash-table-count *projects*))
    (retrieve *storage* 'project T))
  (sort (alexandria:hash-table-values *projects*) #'string< :key #'name))
