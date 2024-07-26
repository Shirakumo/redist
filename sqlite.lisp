(in-package #:org.shirakumo.redist)

(defclass sqlite (storage)
  ((file :initarg :file :initform (make-pathname :name "distinfo" :type "db" :defaults (distinfo-file)) :accessor file)
   (connection :accessor connection)))

(defmethod initialize-instance :after ((*storage* sqlite) &key (if-does-not-exist :create))
  (let ((file (uiop:native-namestring (file *storage*))))
    (unless (probe-file file)
      (ecase if-does-not-exist
        (:error (error "Sqlite database file~%  ~a~%does not exist." file))
        (:create)
        ((NIL) (return-from initialize-instance NIL))))
    (unless (cffi:foreign-library-loaded-p 'sqlite-ffi::sqlite3-lib)
      (cffi:load-foreign-library 'sqlite-ffi::sqlite3-lib))
    (setf (connection *storage*) (sqlite:connect file))
    (dolist (statement (cl-ppcre:split "\\s*;\\s*" #.(alexandria:read-file-into-string
                                                      (asdf:system-relative-pathname :redist "schema.sql"))))
      (query_ statement))))

(defun query (sql &rest params)
  (apply #'sqlite:execute-to-list (connection *storage*) sql params))

(defun query1 (sql &rest params)
  (mapcar #'first (apply #'sqlite:execute-to-list (connection *storage*) sql params)))

(defun query_ (sql &rest params)
  (apply #'sqlite:execute-non-query (connection *storage*) sql params))

(defun update-sqlite (table ids values &key (if-exists :update))
  (let* ((connection (connection *storage*))
         (id (apply #'sqlite:execute-single connection (format NIL "SELECT ID FROM ~a WHERE ~{~a=?~^ AND ~}" table ids)
                    (loop for id in ids collect (getf values id)))))
    (cond ((null id)
           (apply #'sqlite:execute-non-query connection (format NIL "INSERT INTO ~a(~{~a~*~^, ~}) ~:* VALUES(~{?~*~*~^, ~})" table values)
                  (loop for (k v) on values by #'cddr collect v))
           (sqlite:last-insert-rowid connection))
          ((eql :update if-exists)
           (apply #'sqlite:execute-non-query connection (format NIL "UPDATE ~a SET ~{~a=?~*~^, ~} WHERE ID=?" table values)
                  (append (loop for (k v) on values by #'cddr collect v) (list id)))
           id)
          ((eql :error if-exists)
           (error "Record already exists on~%  ~a~%matching~%  ~a" table ids))
          ((eql :ignore if-exists)
           id)
          ((eql NIL if-exists)
           NIL))))

(defun refill (table id-field id value-field values)
  (query_ (format NIL "DELETE FROM ~a WHERE ~a=?" table id-field) id)
  (dolist (value values)
    (query_ (format NIL "INSERT INTO ~a(~a,~a) VALUES(?,?)" table id-field value-field)
            id value)))

(defmacro do-select (vars (query &rest args) &body body)
  `(loop for ,vars in (query ,(format NIL "SELECT ~{~a~^,~} ~a" vars query) ,@args)
         do (progn ,@body)))

(defmethod retrieve ((*storage* sqlite) (object (eql 'dist)) (all (eql T)))
  (do-select (id name url) ("FROM dists")
    (let ((object (ensure-instance (gethash name *dists*) 'dist
                                   :id id :name name :url url)))
      (setf (dist name) object))))

(defmethod retrieve ((*storage* sqlite) (object (eql 'dist)) (name string))
  (destructuring-bind (id name url) (or (query1 "SELECT id,name,url FROM dists WHERE name = ?" name)
                                        (return-from retrieve NIL))
    (let ((object (ensure-instance (gethash name *dists*) 'dist
                                   :id id :name name :url url)))
      (setf (dist name) object))))

(defmethod retrieve ((*storage* sqlite) (object dist) (slot (eql 'projects)))
  (setf (excluded-paths object) (query1 "SELECT path FROM dist_excluded_paths WHERE dist=?" (id object))))

(defmethod retrieve ((*storage* sqlite) (object dist) (slot (eql 'excluded-paths)))
  (setf (projects object) (query1 "SELECT p.name FROM projects AS p INNER JOIN dist_projects AS dp ON p.ID = dp.project WHERE dp.dist = ?" (id object))))

(defmethod retrieve ((*storage* sqlite) (object dist) (slot (eql 'releases)))
  (let ((releases (if (slot-boundp object slot) (slot-value object slot) ())))
    (do-select (id version timestamp) ("FROM dist_releases WHERE dist=?" (id object))
      (pushnew (ensure-instance (find id releases :key #'id) 'release
                                :id id :dist object :version version :timestamp timestamp)
               releases))
    (setf (releases object) releases)))

(defmethod retrieve ((*storage* sqlite) (object release) (slot (eql 'projects)))
  (setf (projects object)
        (loop for (name version) in (query "SELECT p.name,pr.version FROM projects AS p
                                            INNER JOIN project_releases AS pr ON p.ID = pr.project
                                            INNER JOIN dist_release_projects AS dr ON pr.ID = dr.project_release 
                                            WHERE dr.dist_release = ?" (id object))
              collect (find-release version (project name)))))

(defmethod retrieve ((*storage* sqlite) (object (eql 'project)) (all (eql T)))
  (do-select (id name source_directory disabled) ("FROM projects")
    (let ((object (ensure-instance (gethash name *projects*) 'project
                                   :id id :name name :source-directory source_directory :disabled-p (< 0 disabled))))
      (setf (project name) object))))

(defmethod retrieve ((*storage* sqlite) (object (eql 'project)) (name string))
  (destructuring-bind (id name source-directory disabled) (or (query1 "SELECT id,name,source_directory,disabled FROM projects WHERE name = ?" name)
                                                              (return-from retrieve NIL))
    (let ((object (ensure-instance (gethash name *projects*) 'project
                                   :id id :name name :source-directory source-directory :disabled-p (< 0 disabled))))
      (setf (project name) object))))

(defmethod retrieve ((*storage* sqlite) (object project) (slot (eql 'sources)))
  (let ((sources ()))
    (do-select (type url initargs) ("FROM project_sources WHERE project=?" (id object))
      (push (list* type url (read-from-string initargs)) sources))
    (setf (sources object) sources)))

(defmethod retrieve ((*storage* sqlite) (object project) (slot (eql 'excluded-systems)))
  (setf (excluded-systems object) (query1 "SELECT system FROM project_excluded_systems WHERE project=?" (id object))))

(defmethod retrieve ((*storage* sqlite) (object project) (slot (eql 'excluded-paths)))
  (setf (excluded-paths object) (query1 "SELECT path FROM project_excluded_paths WHERE project=?" (id object))))

(defmethod retrieve ((*storage* sqlite) (object project) (slot (eql 'releases)))
  (let ((releases (if (slot-boundp object slot) (slot-value object slot) ())))
    (do-select (id version archive_md5 source_sha1) ("FROM project_releases WHERE project=?" (id object))
      (pushnew (ensure-instance (find id releases :key #'id) 'project-release
                                :id id :project object :version version :archive-md5 archive_md5 :source-sha1 source_sha1)
               releases))
    (setf (releases object) releases)))

(defmethod retrieve ((*storage* sqlite) (object project-release) (slot (eql 'source-files)))
  (setf (source-files object) (query1 "SELECT path FROM project_release_source_files WHERE project_release=?" (id object))))

(defmethod retrieve ((*storage* sqlite) (object project-release) (slot (eql 'systems)))
  (let ((systems (if (slot-boundp object slot) (slot-value object slot) ())))
    (do-select (id name file) ("FROM project_release_systems WHERE project_release=?" (id object))
      (pushnew (ensure-instance (find id systems :key #'id) 'system
                                :id id :name name :project object :file file :dependencies (query1 "SELECT dependency FROM project_release_system_dependencies WHERE system=?" id))
               systems))
    (setf (systems object) systems)))

(defmethod store ((*storage* sqlite) (object dist) (all (eql T)))
  (setf (id object) (update-sqlite "dists" '(:name) (list :name (name object) :url (url object))))
  (store *storage* object 'projects)
  (store *storage* object 'excluded-paths)
  (store *storage* object 'releases))

(defmethod store ((*storage* sqlite) (object dist) (slot (eql 'projects)))
  (query_ "DELETE FROM dist_projects WHERE dist=?" (id object))
  (dolist (project (projects object))
    (query_ "INSERT INTO dist_projects(dist,project) VALUES(?, ?)" (id object) (id project))))

(defmethod store ((*storage* sqlite) (object dist) (slot (eql 'excluded-paths)))
  (refill "dist_excluded_paths" :dist (id object) :path
          (mapcar #'namestring (excluded-paths object))))

(defmethod store ((*storage* sqlite) (object dist) (slot (eql 'releases)))
  (dolist (release (releases object))
    (store *storage* release T)))

(defmethod store ((*storage* sqlite) (object release) (slot (eql T)))
  (setf (id object) (update-sqlite "dist_releases" '(:dist :version)
                                   (list :dist (id (dist object))
                                         :version (version object)
                                         :timestamp (timestamp object))))
  (store *storage* object 'projects))

(defmethod store ((*storage* sqlite) (object release) (slot (eql 'projects)))
  (refill "dist_release_projects" :dist_release (id object) :project_release
          (mapcar #'id (projects object))))

(defmethod store ((*storage* sqlite) (object project) (all (eql T)))
  (setf (id object) (update-sqlite "projects" '(:name) (list :name (name object)
                                                             :source_directory (relpath (source-directory object) (default-source-directory))
                                                             :disabled (if (disabled-p object) 1 0))))
  (store *storage* object 'sources)
  (store *storage* object 'excluded-systems)
  (store *storage* object 'excluded-paths))

(defmethod store ((*storage* sqlite) (object project) (slot (eql 'sources)))
  (query_ "DELETE FROM project_sources WHERE project=?" (id object))
  (dolist (source (sources object))
    (destructuring-bind (type url . args) (serialize source)
      (query_ "INSERT INTO project_sources(project,type,url,initargs) VALUES(?,?,?,?)"
              (id object) (string type) url (prin1-to-string args)))))

(defmethod store ((*storage* sqlite) (object project) (slot (eql 'excluded-systems)))
  (refill "project_excluded_systems" :project (id object) :system (excluded-systems object)))

(defmethod store ((*storage* sqlite) (object project) (slot (eql 'excluded-paths)))
  (refill "project_excluded_paths" :project (id object) :path (mapcar #'namestring (excluded-paths object))))

(defmethod store ((*storage* sqlite) (object project) (slot (eql 'releases)))
  (dolist (release (releases object))
    (store *storage* release T)))

(defmethod store ((*storage* sqlite) (object project-release) (slot (eql T)))
  (setf (id object) (update-sqlite "project_releases" '(:project :version)
                                   (list :project (id (project object))
                                         :version (version object)
                                         :archive_md5 (archive-md5 object)
                                         :source_sha1 (source-sha1 object))))
  (store *storage* object 'source-files)
  (store *storage* object 'systems))

(defmethod store ((*storage* sqlite) (object project-release) (slot (eql 'source-files)))
  (refill "project_release_source_files" :project_release (id object) :path
          (loop for path in (source-files object)
                collect (relpath path (source-directory object)))))

(defmethod store ((*storage* sqlite) (object project-release) (slot (eql 'systems)))
  (dolist (system (systems object))
    (store *storage* system T)))

(defmethod store ((*storage* sqlite) (object system) (all (eql T)))
  (setf (id object) (update-sqlite "project_release_systems" '(:project_release :name)
                                   (list :project-release (id (project object))
                                         :name (name object)
                                         :file (namestring (file object)))))
  (store *storage* object 'dependencies))

(defmethod store ((*storage* sqlite) (object system) (slot (eql 'dependencies)))
  (refill "project_release_system_dependencies" :system (id object) :dependency
          (dependencies object)))
