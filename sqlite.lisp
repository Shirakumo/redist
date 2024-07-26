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

(defun update-sqlite (table ids values &key (if-exists :update))
  (let ((id (apply #'sqlite:execute-single *sqlite* (format NIL "SELECT ID FROM ~a WHERE ~{~a=?~^ AND ~}" table ids)
                   (loop for id in ids collect (getf values id)))))
    (cond ((null id)
           (apply #'sqlite:execute-non-query *sqlite* (format NIL "INSERT INTO ~a(~{~a~*~^, ~}) ~:* VALUES(~{?~*~*~^, ~})" table values)
                  (loop for (k v) on values by #'cddr collect v))
           (sqlite:last-insert-rowid *sqlite*))
          ((eql :update if-exists)
           (apply #'sqlite:execute-non-query *sqlite* (format NIL "UPDATE ~a SET ~{~a=?~*~^, ~} WHERE ID=?" table values)
                  (append (loop for (k v) on values by #'cddr collect v) (list id)))
           id)
          ((eql :error if-exists)
           (error "Record already exists on~%  ~a~%matching~%  ~a" table ids))
          ((eql :ignore if-exists)
           id)
          ((eql NIL if-exists)
           NIL))))

(defun persist-sqlite (&key (file (sqlite-file)))
  (flet ((update (table ids &rest values)
           (update-sqlite table ids values :if-exists :update))
         (insert (table ids &rest values)
           (update-sqlite table ids values :if-exists NIL))
         (refill (table id-field id value-field values)
           (query_ (format NIL "DELETE FROM ~a WHERE ~a=?" table id-field) id)
           (dolist (value values)
             (query_ (format NIL "INSERT INTO ~a(~a,~a) VALUES(?,?)" table id-field value-field)
                     id value)))
         (relpath (path parent)
           (namestring (pathname-utils:enough-pathname path parent))))
    (loop for project in (list-projects)
          for id = (update "projects" '(:name)
                           :name (name project)
                           :source_directory (relpath (source-directory project) (default-source-directory))
                           :disabled (if (disabled-p project) 1 0))
          do (query_ "DELETE FROM project_sources WHERE project=?" id)
             (dolist (source (sources project))
               (destructuring-bind (type url . args) (serialize source)
                 (query_ "INSERT INTO project_sources(project,type,url,initargs) VALUES(?,?,?,?)"
                         id (string type) url (prin1-to-string args))))
             (refill "project_excluded_systems" :project id :system (excluded-systems project))
             (refill "project_excluded_paths" :project id :path (mapcar #'namestring (excluded-paths project)))
             (loop for release in (releases project)
                   for rid = (insert "project_releases" '(:project :version)
                                     :project id
                                     :version (version release)
                                     :archive_md5 (archive-md5 release)
                                     :source_sha1 (source-sha1 release))
                   do (when rid
                        (when (source-files release)
                          (refill "project_release_source_files" :project_release rid :path (loop for path in (source-files release)
                                                                                                  collect (relpath path (source-directory project)))))
                        (loop for system in (systems release)
                              for sid = (insert "project_release_systems" '(:project_release :name)
                                                :project_release rid
                                                :name (name system)
                                                :file (namestring (file system)))
                              do (when sid
                                   (refill "project_release_system_dependencies" :system sid :dependency (dependencies system)))))))
    (loop for dist in (list-dists)
          for id = (update "dists" '(:name)
                           :name (name dist)
                           :url (url dist))
          do (query_ "DELETE FROM dist_projects WHERE dist=?" id)
             (dolist (project (projects dist))
               (query_ "INSERT INTO dist_projects(dist,project)
                          SELECT ?,ID FROM projects WHERE name=?" id (name project)))
             (refill "dist_excluded_paths" :dist id :path (mapcar #'namestring (excluded-paths dist)))
             (loop for release in (releases dist)
                   for rid = (insert "dist_releases" '(:dist :version)
                                     :dist id
                                     :version (version release)
                                     :timestamp (timestamp release))
                   do (when rid
                        (dolist (project (projects release))
                          (query_ "INSERT INTO dist_release_projects(dist_release,project_release)
                                     SELECT ?,pr.ID FROM project_releases AS pr
                                                  INNER JOIN projects AS p ON pr.project = p.ID
                                                  WHERE pr.version = ? AND p.name = ?"
                                  rid (version project) (name (project project)))))))))
