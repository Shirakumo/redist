(in-package #:org.shirakumo.redist)

(defvar *dists* (make-hash-table :test 'equalp))
(defvar *projects* (make-hash-table :test 'equalp))
(defvar *storage-file* NIL)
(defvar *storage* NIL)

(defun storage-file ()
  (flet ((try (dir file)
           (when dir (probe-file (merge-pathnames file dir))))
         (use (dir file)
           (when dir (merge-pathnames file dir))))
    (or *storage-file*
        (try *default-source-directory* "../distinfo.db")
        (try *default-source-directory* "../distinfo.lisp")
        (try *default-output-directory* "../distinfo.db")
        (try *default-output-directory* "../distinfo.lisp")
        (try (user-homedir-pathname) "dist/distinfo.db")
        (try (user-homedir-pathname) "dist/distinfo.lisp")
        ;; Default
        (use *default-source-directory* "../distinfo.lisp")
        (use *default-output-directory* "../distinfo.lisp")
        (use (user-homedir-pathname) "dist/distinfo.lisp"))))

(defclass storage ()
  ((file :initarg :file :initform (arg! :file) :accessor file)))

(defgeneric open-storage (file type))
(defgeneric retrieve (storage object slot))
(defgeneric store (storage object slot))

(defmethod open-storage ((file string) type)
  (open-storage (uiop:parse-native-namestring file) type))

(defmethod open-storage ((pathname pathname) (type (eql T)))
  (open-storage pathname (intern (string-upcase (pathname-type pathname)) "KEYWORD")))

(defun try-open-storage (&optional (file (storage-file)))
  (let ((truename (probe-file file)))
    (when truename
      (setf *storage* (open-storage truename T)))))

(defclass stored-object ()
  ((id :initarg :id :writer (setf id))))

(defmethod stored-p ((object stored-object))
  (slot-boundp object 'id))

(defmethod id ((object stored-object))
  (when (and *storage* (not (slot-boundp object 'id)))
    (store *storage* object T))
  (slot-value object 'id))

(defmethod c2mop:slot-value-using-class :before ((class c2mop:standard-class) (object stored-object) slotd)
  (unless (c2mop:slot-boundp-using-class class object slotd)
    (when *storage* (retrieve *storage* object (c2mop:slot-definition-name slotd)))))

(defmethod (setf c2mop:slot-value-using-class) :before (value (class c2mop:standard-class) (object stored-object) slotd)
  (when (c2mop:slot-boundp-using-class class object slotd)
    (when *storage* (store *storage* object (c2mop:slot-definition-name slotd)))))

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
  (when (and *storage* (= 0 (hash-table-count *dists*)))
    (retrieve *storage* 'dist T))
  (sort (alexandria:hash-table-values *dists*) #'string< :key #'name))

(defmethod project ((name string))
  (or (gethash name *projects*)
      (retrieve *storage* 'project name)))

(defmethod (setf project) (project (name string))
  (setf (gethash name *projects*) project))

(defun list-projects ()
  (when (and *storage* (= 0 (hash-table-count *projects*)))
    (retrieve *storage* 'project T))
  (sort (alexandria:hash-table-values *projects*) #'string< :key #'name))
