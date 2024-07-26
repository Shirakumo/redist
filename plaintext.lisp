(in-package #:org.shirakumo.redist)

(defmethod open-storage ((file pathname) (type (eql :lisp)))
  (make-instance 'plaintext :file file))

(defclass plaintext (storage)
  ((file :initform (make-pathname :name "distinfo" :type "db" :defaults (storage-file)))
   (dir :accessor dir)
   (id-counter :initform 0 :accessor id-counter)))

(defmethod initialize-instance :after ((*storage* plaintext) &key (if-does-not-exist :create))
  (let ((file (file *storage*)))
    (unless (probe-file file)
      (ecase if-does-not-exist
        (:error (error "Storage file~%  ~a~%does not exist." file))
        (:create (with-open-file (stream file :direction :output)))
        ((NIL) (return-from initialize-instance NIL))))
    (load file)
    (unless (slot-boundp *storage* 'dir)
      (setf (dir *storage*) (merge-pathanmes ".distinfo/" (make-pathname :name NIL :type NIL :defaults file))))))

(defmethod store :after ((*storage* plaintext) (all (eql T)) (all2 (eql T)))
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*package* #.*package*)
            (*print-case* :downcase)
            (*print-right-margin* 80)
            (*print-readably* NIL))
        (format stream "~&;;;;; Distinfo compiled automatically")
        (terpri stream) (pprint '(in-package #.(package-name *package*)) stream)
        (terpri stream) (pprint '(setf (id-counter *storage*) ,(id-counter *storage*)))
        (terpri stream) (pprint '(setf (dir *storage*) ,(dir *storage*)))
        (terpri stream)))))

(defmethod store :before ((*storage* plaintext) (object stored-object) (slot (eql T)))
  (unless (stored-p object)
    (setf (id object) (incf (id-counter *storage*)))))

(defun plaintext-file (object &optional slot)
  (merge-pathnames
   (make-pathname :name (princ-to-string (id object))
                  :type (when slot (string-downcase slot))
                  :directory (list :relative (string-downcase (type-of object))))
   (dir *storage*)))

(defun store-plaintext (object &rest fields)
  (let ((file (plaintext-file object)))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*package* #.*package*)
              (*print-case* :downcase)
              (*print-right-margin* 80)
              (*print-readably* NIL))
          (format stream "~&(~s ~s" :id (id object))
          (loop for (k v) on fields by #'cddr
                do (format stream "~& ~s ~s" k v))
          (format stream ")~%"))))))

(defun store-slot (object slot value)
  (let ((file (plaintext-file object slot)))
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*package* #.*package*)
              (*print-case* :downcase)
              (*print-right-margin* 80)
              (*print-readably* NIL))
          (format stream "~&~s~%" value))))))

(defun retrieve-plaintext (object &optional slot)
  (let ((file (plaintext-file object slot)))
    (with-open-file (stream file :direction :input)
      (let ((*package* #.*package*))
        (read stream)))))

(defmethod retrieve ((*storage* plaintext) (object dist) (name string))
  )

(defmethod retrieve ((*storage* plaintext) (object (eql 'dist)) (all (eql T)))
  )

(defmethod retrieve ((*storage* plaintext) (object dist) (slot (eql T)))
  )

(defmethod retrieve ((*storage* plaintext) (object dist) (slot (eql 'excluded-paths)))
  (setf (excluded-paths object) (retrieve-plaintext object slot)))

(defmethod retrieve ((*storage* plaintext) (object dist) (slot (eql 'projects)))
  )

(defmethod retrieve ((*storage* plaintext) (object dist) (slot (eql 'releases)))
  )

(defmethod retrieve ((*storage* plaintext) (object release) (slot (eql T)))
  )

(defmethod retrieve ((*storage* plaintext) (object release) (slot (eql 'projects)))
  )

(defmethod retrieve ((*storage* plaintext) (object project) (name string))
  )

(defmethod retrieve ((*storage* plaintext) (object (eql 'project)) (all (eql T)))
  )

(defmethod retrieve ((*storage* plaintext) (object project) (slot (eql T)))
  )

(defmethod retrieve ((*storage* plaintext) (object project) (slot (eql 'excluded-systems)))
  (setf (excluded-systems object) (retrieve-plaintext object slot)))

(defmethod retrieve ((*storage* plaintext) (object project) (slot (eql 'excluded-paths)))
  (setf (excluded-paths object) (retrieve-plaintext object slot)))

(defmethod retrieve ((*storage* plaintext) (object project) (slot (eql 'releases)))
  )

(defmethod retrieve ((*storage* plaintext) (object project-release) (slot (eql T)))
  )

(defmethod retrieve ((*storage* plaintext) (object project-release) (slot (eql 'source-files)))
  (setf (source-files object) (retrieve-plaintext object slot)))

(defmethod store ((*storage* plaintext) (object dist) (slot (eql T)))
  (store-plaintext object
                   :name (name object)
                   :url (url object))
  (store *storage* object 'excluded-paths)
  (store *storage* object 'projects)
  (store *storage* object 'releases))

(defmethod store ((*storage* plaintext) (object dist) (slot (eql 'excluded-paths)))
  (store-slot object slot (excluded-paths object)))

(defmethod store ((*storage* plaintext) (object dist) (slot (eql 'projects)))
  (store-slot object slot (mapcar #'id (projects object))))

(defmethod store ((*storage* plaintext) (object dist) (slot (eql 'releases)))
  (dolist (release (releases object))
    (store *storage* release T))
  (store-slot object slot (mapcar #'id (releases object))))

(defmethod store ((*storage* plaintext) (object release) (slot (eql T)))
  (store-plaintext object
                   :dist (id (dist object))
                   :version (version object)
                   :timestamp (timestamp object))
  (store *storage* object 'projects))

(defmethod store ((*storage* plaintext) (object release) (slot (eql 'projects)))
  (dolist (project (projects object))
    (store *storage* project T))
  (store-slot object slot (mapcar #'id (projects object))))

(defmethod store ((*storage* plaintext) (object project) (all (eql T)))
  (store-plaintext object
                   :name (name object)
                   :source-directory (relpath (source-directory object) (default-source-directory))
                   :disabled-p (disabled-p object)
                   :sources (mapcar #'serialize (sources object)))
  (store *storage* object 'excluded-systems)
  (store *storage* object 'excluded-paths)
  (store *storage* object 'releases))

(defmethod store ((*storage* plaintext) (object project) (slot (eql 'excluded-systems)))
  (store-slot object slot (excluded-systems object)))

(defmethod store ((*storage* plaintext) (object project) (slot (eql 'excluded-paths)))
  (store-slot object slot (excluded-paths object)))

(defmethod store ((*storage* plaintext) (object project) (slot (eql 'releases)))
  (dolist (release (releases object))
    (store *storage* release T))
  (store-slot object slot (mapcar #'id (releases object))))

(defmethod store ((*storage* plaintext) (object project-release) (all (eql T)))
  (store-plaintext object
                   :project (id (project object))
                   :version (version object)
                   :archive-md5 (archive-md5 object)
                   :source-sha1 (source-sha1 object)
                   :systems (loop for system in (systems object)
                                  collect (list :name (name object)
                                                :file (file object)
                                                :dependencies (dependencies object))))
  (store *storage* object 'source-files))

(defmethod store ((*storage* plaintext) (object project-release) (slot (eql 'source-files)))
  (store-slot object slot (source-files object)))
