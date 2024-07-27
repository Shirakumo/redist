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
      (setf (dir *storage*) (merge-pathnames ".distinfo/" (make-pathname :name NIL :type NIL :defaults file))))))

(defmethod store :after ((*storage* plaintext) (all (eql T)) (all2 (eql T)))
  (with-open-file (stream (file *storage*) :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*package* #.*package*)
            (*print-case* :downcase)
            (*print-right-margin* 80)
            (*print-readably* NIL))
        (format stream "~&;;;;; Distinfo compiled automatically")
        (terpri stream) (pprint `(in-package #.(package-name *package*)) stream)
        (terpri stream) (pprint `(setf (id-counter *storage*) ,(id-counter *storage*)) stream)
        (terpri stream) (pprint `(setf (dir *storage*) ,(dir *storage*)) stream)
        (terpri stream)))))

(defmethod store :before ((*storage* plaintext) (object stored-object) (slot (eql T)))
  (unless (stored-p object)
    (setf (id object) (incf (id-counter *storage*)))))

(defun plaintext-file (type id &optional slot)
  (merge-pathnames
   (make-pathname :name (princ-to-string id)
                  :type (when slot (string-downcase slot))
                  :directory (list :relative (string-downcase type)))
   (dir *storage*)))

(defun store-plaintext (object &rest fields)
  (let ((file (plaintext-file (type-of object) (id object))))
    (ensure-directories-exist file)
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
  (let ((file (plaintext-file (type-of object) (id object) slot)))
    (ensure-directories-exist file)
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (with-standard-io-syntax
        (let ((*package* #.*package*)
              (*print-case* :downcase)
              (*print-right-margin* 80)
              (*print-readably* NIL))
          (format stream "~&~s~%" value))))))

(defun read-plaintext (file)
  (with-open-file (stream file :direction :input)
    (let ((*package* #.*package*))
      (read stream))))

(defun retrieve-plaintext (object &optional slot)
  (read-plaintext (plaintext-file (type-of object) (id object) slot)))

(defun retrieve-listed (object slot type existing)
  (loop for id in (retrieve-plaintext object slot)
        collect (apply #'ensure-instance (find id existing :key #'id) type
                       (read-plaintext (plaintext-file type id)))))

(defmethod retrieve ((*storage* plaintext) (object dist) (name string))
  (let ((id (pathname-name (truename (plaintext-file 'dist name)))))
    (ensure-instance (gethash name *dists*) 'dist :id id :name name)))

(defmethod retrieve ((*storage* plaintext) (object (eql 'dist)) (all (eql T)))
  (dolist (file (directory (plaintext-file 'dist "*")))
    (when (every #'digit-char-p (pathname-name file))
      (let* ((args (read-plaintext file))
             (name (getf args :name)))
        (setf (dist name) (apply #'ensure-instance (gethash name *dists*) 'dist args))))))

(defmethod retrieve ((*storage* plaintext) (object dist) (slot (eql T)))
  (apply #'reinitialize-instance object (retrieve-plaintext object)))

(defmethod retrieve ((*storage* plaintext) (object dist) (slot (eql 'excluded-paths)))
  (setf (excluded-paths object) (retrieve-plaintext object slot)))

(defmethod retrieve ((*storage* plaintext) (object dist) (slot (eql 'projects)))
  (setf (projects object) (retrieve-listed object slot 'project (projects object))))

(defmethod retrieve ((*storage* plaintext) (object dist) (slot (eql 'releases)))
  (setf (releases object) (retrieve-listed object slot 'release (releases object))))

(defmethod retrieve ((*storage* plaintext) (object release) (slot (eql T)))
  (apply #'reinitialize-instance object (retrieve-plaintext object)))

(defmethod retrieve ((*storage* plaintext) (object release) (slot (eql 'projects)))
  (setf (projects object) (retrieve-listed object slot 'project (projects object))))

(defmethod retrieve ((*storage* plaintext) (object project) (name string))
  (let ((id (pathname-name (truename (plaintext-file 'project name)))))
    (ensure-instance (gethash name *projects*) 'project :id id :name name)))

(defmethod retrieve ((*storage* plaintext) (object (eql 'project)) (all (eql T)))
  (dolist (file (directory (plaintext-file 'project "*")))
    (when (every #'digit-char-p (pathname-name file))
      (let* ((args (read-plaintext file))
             (name (getf args :name)))
        (setf (project name) (apply #'ensure-instance (gethash name *projects*) 'project args))))))

(defmethod retrieve ((*storage* plaintext) (object project) (slot (eql T)))
  (apply #'reinitialize-instance object (retrieve-plaintext object)))

(defmethod retrieve ((*storage* plaintext) (object project) (slot (eql 'excluded-systems)))
  (setf (excluded-systems object) (retrieve-plaintext object slot)))

(defmethod retrieve ((*storage* plaintext) (object project) (slot (eql 'excluded-paths)))
  (setf (excluded-paths object) (retrieve-plaintext object slot)))

(defmethod retrieve ((*storage* plaintext) (object project) (slot (eql 'releases)))
  (setf (releases object) (retrieve-listed object slot 'project-release (releases object))))

(defmethod retrieve ((*storage* plaintext) (object project-release) (slot (eql T)))
  (apply #'reinitialize-instance object (retrieve-plaintext object)))

(defmethod retrieve ((*storage* plaintext) (object project-release) (slot (eql 'source-files)))
  (setf (source-files object) (retrieve-plaintext object slot)))

(defmethod store ((*storage* plaintext) (object dist) (slot (eql T)))
  (store-plaintext object
                   :name (name object)
                   :url (url object))
  (let ((link (plaintext-file 'dist (name object))))
    (when (probe-file link) (delete-file link))
    (filesystem-utils:create-symbolic-link link (plaintext-file 'dist (id object))))
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
  (store-slot object slot (mapcar #'id (projects object))))

(defmethod store ((*storage* plaintext) (object project) (all (eql T)))
  (store-plaintext object
                   :name (name object)
                   :source-directory (relpath (source-directory object) (default-source-directory))
                   :disabled-p (disabled-p object)
                   :sources (mapcar #'serialize (sources object)))
  (let ((link (plaintext-file 'project (name object))))
    (when (probe-file link) (delete-file link))
    (filesystem-utils:create-symbolic-link link (plaintext-file 'project (id object))))
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
                                  collect (list :name (name system)
                                                :file (file system)
                                                :dependencies (dependencies system))))
  (store *storage* object 'source-files))

(defmethod store ((*storage* plaintext) (object project-release) (slot (eql 'source-files)))
  (store-slot object slot (source-files object)))
