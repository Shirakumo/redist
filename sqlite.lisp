#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.redist)

(defvar *sqlite-file* NIL)
(defvar *sqlite* NIL)

(defun sqlite-file ()
  (or *sqlite-file*
      (make-pathname :name "distinfo" :type "db" :defaults (distinfo-file))))

(defun call-with-sqlite (function file &key (if-does-not-exist :create))
  (cond (*sqlite*
         (funcall function))
        (T
         (unless (probe-file file)
           (ecase if-does-not-exist
             (:error (error "Sqlite database file~%  ~a~%does not exist." file))
             (:create)
             ((NIL) (return-from call-with-sqlite NIL))))
         (unless (cffi:foreign-library-loaded-p 'sqlite-ffi::sqlite3-lib)
           (cffi:load-foreign-library 'sqlite-ffi::sqlite3-lib))
         (sqlite:with-open-database (*sqlite* file)
           (sqlite:with-transaction *sqlite*
             (funcall function))))))

(defmacro with-sqlite ((file &rest args) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk ()
              (flet ((query (sql &rest params)
                       (apply #'sqlite:execute-to-list *sqlite* sql params))
                     (query1 (sql &rest params)
                       (mapcar #'first (apply #'sqlite:execute-to-list *sqlite* sql params)))
                     (query_ (sql &rest params)
                       (apply #'sqlite:execute-non-query *sqlite* sql params)))
                (declare (ignorable #'query #'query1 #'query_))
                ,@body)))
       (call-with-sqlite #',thunk ,file ,@args))))

(defmacro do-select (vars (query &rest args) &body body)
  `(loop for ,vars in (query ,(format NIL "SELECT ~{~a~^,~} ~a" vars query) ,@args)
         do (progn ,@body)))

(defun init-sqlite (&key (file (sqlite-file)) (if-does-not-exist :create))
  (with-sqlite (file :if-does-not-exist if-does-not-exist)
    (dolist (statement (cl-ppcre:split "\\s*;\\s*" #.(alexandria:read-file-into-string
                                     (asdf:system-relative-pathname :redist "schema.sql"))))
      (query_ statement))))

(defun restore-sqlite (&key (file (sqlite-file)) (if-does-not-exist :error) restore-source-files)
  ;; FIXME: add a "ignore if exists" option for releases
  (with-sqlite (file :if-does-not-exist if-does-not-exist)
    (init-sqlite)
    (do-select (id name source_directory disabled) ("FROM projects")
      (let* ((sources ())
             (project (ensure-instance (project name) 'project :name name :source-directory source_directory)))
        (do-select (type url initargs) ("FROM project_sources WHERE project=?" id)
          (push (list* type url (read-from-string initargs))
                sources))
        (ensure-instance project 'project
                         :disabled-p (< 0 disabled)
                         :excluded-systems (query1 "SELECT system FROM project_excluded_systems WHERE project=?" id)
                         :excluded-paths (query1 "SELECT path FROM project_excluded_paths WHERE project=?" id)
                         :sources sources)
        (setf (project (name project)) project)
        (do-select (id version archive_md5 source_sha1) ("FROM project_releases WHERE project=?" id)
          (let ((systems ()))
            (do-select (id name file) ("FROM project_release_systems WHERE project_release=?" id)
              (push (list name :file file :dependencies (query1 "SELECT dependency FROM project_release_system_dependencies WHERE system=?" id))
                    systems))
            (pushnew (ensure-release (list version
                                           :archive-md5 archive_md5
                                           :source-sha1 source_sha1
                                           :source-files (when restore-source-files
                                                           (query1 "SELECT path FROM project_release_source_files WHERE project_release=?" id))
                                           :systems systems)
                                     project)
                     (releases project))))))
    (do-select (id name url) ("FROM dists")
      (let ((dist (ensure-instance (dist name) 'dist
                                   :name name :url url
                                   :projects (query1 "SELECT p.name FROM projects AS p INNER JOIN dist_projects AS dp ON p.ID = dp.project WHERE dp.dist = ?" id)
                                   :excluded-paths (query1 "SELECT path FROM dist_excluded_paths WHERE dist=?" id))))
        (setf (dist (name dist)) dist)
        (do-select (id version timestamp) ("FROM dist_releases WHERE dist=?" id)
          (pushnew (ensure-release (list version
                                         :timestamp timestamp
                                         :projects (loop for (name version) in (query "SELECT p.name,pr.version FROM projects AS p
                                                                 INNER JOIN project_releases AS pr ON p.ID = pr.project
                                                                 INNER JOIN dist_release_projects AS dr ON pr.ID = dr.project_release 
                                                                 WHERE dr.dist_release = ?" id)
                                                         collect (find-release version (project name))))
                                   dist)
                   (releases dist)))))))

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
  (with-sqlite (file)
    (init-sqlite)
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
                                    rid (version project) (name (project project))))))))))
