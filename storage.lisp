(in-package #:org.shirakumo.redist)

(defvar *dists* (make-hash-table :test 'equalp))
(defvar *projects* (make-hash-table :test 'equalp))
(defvar *storage-file* NIL)
(defvar *storage* NIL)
(defvar *retrieving* NIL)

(defun storage-file ()
  (flet ((try (dir file)
           (when dir (merge-pathnames file (pathname-utils:to-directory dir)))))
    (let ((dirs (list (try *default-source-directory* "../")
                      (try *default-output-directory* "../")
                      (try (user-homedir-pathname) "dist/")
                      (try (probe-file (first (uiop:raw-command-line-arguments))) ""))))
      (or *storage-file*
          (loop for dir in dirs
                thereis (loop for type in (list-storage-file-types)
                              thereis (probe-file (make-pathname :name "distinfo" :type type :defaults dir))))
          (loop for dir in dirs
                thereis (when dir (make-pathname :name "distinfo" :type (first (list-storage-file-types)) :defaults dir)))))))

(defclass storage ()
  ((file :initarg :file :initform (arg! :file) :accessor file)))

(defgeneric open-storage (file type))
(defgeneric retrieve (storage object slot))
(defgeneric store (storage object slot))

(defun list-storage-file-types ()
  (loop for method in (c2mop:generic-function-methods #'open-storage)
        for type = (second (c2mop:method-specializers method))
        when (and (typep type 'c2mop:eql-specializer) (not (eql T (c2mop:eql-specializer-object type))))
        collect (string-downcase (c2mop:eql-specializer-object type))))

(defmethod open-storage ((file string) type)
  (open-storage (pathname-utils:parse-native-namestring file) type))

(defmethod open-storage ((pathname pathname) (type (eql T)))
  (open-storage pathname (intern (string-upcase (pathname-type pathname)) "KEYWORD")))

(defun try-open-storage (&optional (file (storage-file)))
  (let ((truename (probe-file file)))
    (when truename
      (setf *storage* (open-storage truename T)))))

(defun ensure-storage (&key (file (storage-file)))
  (or *storage*
      (progn (verbose "Creating storage in ~a" file)
             (setf *storage* (open-storage (ensure-directories-exist file) T)))))

(defmacro without-storing (&body body)
  `(let ((*retrieving* T)) ,@body))

(defclass stored-object ()
  ((id :initarg :id :writer (setf id))))

(defmethod stored-p ((object stored-object))
  (slot-boundp object 'id))

(defmethod id ((object stored-object))
  (when (and *storage* (not (slot-boundp object 'id)))
    (store *storage* object T))
  (slot-value object 'id))

(defmethod initialize-instance :around ((object stored-object) &key)
  (without-storing (call-next-method)))

(defmethod shared-initialize :around ((object stored-object) slots &key)
  (if (stored-p object)
      (call-next-method)
      (without-storing (call-next-method))))

(defmethod c2mop:slot-value-using-class :before ((class c2mop:standard-class) (object stored-object) slotd)
  (when (and *storage* (not (c2mop:slot-boundp-using-class class object slotd)))
    (retrieve *storage* object (c2mop:slot-definition-name slotd))))

(defmethod (setf c2mop:slot-value-using-class) :before (value (class c2mop:standard-class) (object stored-object) slotd)
  (when (and *storage* (not *retrieving*) (c2mop:slot-boundp-using-class class object slotd))
    (store *storage* object (c2mop:slot-definition-name slotd))))

(defmethod retrieve :around ((storage storage) (object stored-object) slot)
  (without-storing
    (call-next-method)))

(defmethod retrieve ((storage storage) (object stored-object) slot))
(defmethod store ((storage storage) (object stored-object) slot))

(defmethod retrieve ((storage storage) (all (eql T)) (all2 (eql T)))
  (retrieve storage 'project T)
  (retrieve storage 'dist T))

(defmethod retrieve ((storage (eql T)) object slot)
  (unless *storage*
    (setf *storage* (or (try-open-storage) (make-instance 'plaintext))))
  (retrieve *storage* object slot))

(defmethod store ((storage (eql T)) object slot)
  (unless *storage*
    (setf *storage* (or (try-open-storage) (make-instance 'plaintext))))
  (store *storage* object slot))

(defmethod store :before ((storage storage) (object stored-object) slot)
  (unless (or (stored-p object) (eql slot T))
    (store storage object T)))

(defmethod store :around ((storage storage) (object stored-object) slot)
  (when (or (eql slot T) (slot-boundp object slot))
    ;; If we haven't retrieved the slot, we don't need to store it either.
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
  (dist (string-downcase name)))

(defmethod dist ((name string))
  (or (gethash name *dists*)
      (when *storage* (retrieve *storage* 'dist name))))

(defmethod (setf dist) (dist (name symbol))
  (setf (dist (string name)) dist))

(defmethod (setf dist) (dist (name string))
  (setf (gethash name *dists*) dist))

(defmethod (setf dist) ((dist null) (name string))
  (remhash dist *dists*)
  NIL)

(defun list-dists ()
  (when (and *storage* (= 0 (hash-table-count *dists*)))
    (retrieve *storage* 'dist T))
  (sort (alexandria:hash-table-values *dists*) #'string< :key #'name))

(defmethod project ((name symbol))
  (project (string-downcase name)))

(defmethod project ((name string))
  (or (gethash name *projects*)
      (when *storage* (retrieve *storage* 'project name))))

(defmethod (setf project) (project (name symbol))
  (setf (project (string name)) project))

(defmethod (setf project) (project (name string))
  (setf (gethash name *projects*) project))

(defmethod (setf project) ((project null) (name string))
  (remhash project *projects*)
  NIL)

(defun list-projects ()
  (when (and *storage* (= 0 (hash-table-count *projects*)))
    (retrieve *storage* 'project T))
  (sort (alexandria:hash-table-values *projects*) #'string< :key #'name))

(defmethod retrieve-all ((defalut (eql T)) thing)
  (retrieve-all *storage* thing))

(defmethod retrieve-all ((storage storage) (all (eql T)))
  (retrieve-all storage 'project)
  (retrieve-all storage 'dist))

(defmethod retrieve-all ((storage storage) (dists (eql 'dist)))
  (retrieve storage 'dist T)
  (loop for object being the hash-values of *dists*
        do (retrieve-all storage object)))

(defmethod retrieve-all ((storage storage) (dists (eql 'project)))
  (retrieve storage 'project T)
  (loop for object being the hash-values of *projects*
        do (retrieve-all storage object)))

(defmethod retrieve-all ((storage storage) (object stored-object))
  (retrieve storage object T)
  (loop for slot in (c2mop:class-slots (class-of object))
        for name = (c2mop:slot-definition-name slot)
        do (when (loop for method in (compute-applicable-methods #'retrieve (list storage object name))
                       thereis (loop for arg in (c2mop:method-specializers method)
                                     thereis (typep arg 'c2mop:eql-specializer)))
             (retrieve storage object name))))

