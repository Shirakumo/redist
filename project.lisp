(in-package #:org.shirakumo.redist)

(defvar *excluded-paths* '(#p".git/"
                           #p".github/"
                           #p".gitignore"
                           #p".gitattributes"
                           #p".svn/"
                           #p".hg/"
                           #p".hgignore"
                           #p".hgtags"
                           #p"CVS/"
                           #p"CVSROOT/"
                           #p"_darcs/"
                           #p".travis.yml"
                           #p"{APPNAME}.asd"
                           #p"/debian/"
                           #p"/unused/"
                           #p"/external/cffi.darcs/"
                           #p"/external/lift.darcs/"
                           #p"/sav/"
                           #p"/1580-frozen/"
                           #p"/Code/fset"
                           #p"/Outdated Demos/"
                           #p"jenkins/"
                           #p"_build/"
                           #p"ext/rt/"
                           #p"asd-generator-data.asd"))

(defvar *default-source-directory* NIL)

(defun default-source-directory ()
  (or *default-source-directory*
      (make-pathname :name NIL :type NIL :defaults (merge-pathnames "sources/" (storage-file)))))

(defclass project (stored-object)
  ((name :initarg :name :initform (arg! :name) :accessor name)
   (source-directory :accessor source-directory)
   (sources :accessor sources)
   (disabled-p :initarg :disabled-p :initform NIL :accessor disabled-p)
   (excluded-systems :initarg :excluded-systems :accessor excluded-systems)
   (excluded-paths :initarg :excluded-paths :accessor excluded-paths)
   (releases :accessor releases)
   (version-cache :initform NIL :accessor version-cache)))

(defmethod shared-initialize :after ((project project) slots &key (releases NIL releases-p) (sources NIL sources-p) source-directory (verbose T))
  (when source-directory
    (setf (source-directory project) (uiop:truenamize (absolutize source-directory (default-source-directory)))))
  (when (and (not (slot-boundp project 'source-directory))
             (not (stored-p project)))
    (setf (source-directory project) (pathname-utils:subdirectory (default-source-directory) (name project))))
  (when releases-p
    (setf (releases project) releases))
  (when sources-p
    (setf (sources project) sources))
  (when (and (sources project)
             (not (disabled-p project))
             (or (not (probe-file (source-directory project)))
                 (empty-directory-p (source-directory project))))
    (restart-case (clone project :verbose verbose)
      (disable ()
        :report "Disable the project"
        (setf (disabled-p project) T)))))

(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type T)
    (format stream "~a ~:[INACTIVE~;ACTIVE~]" (name project) (not (disabled-p project)))))

(defmethod describe-object ((project project) stream)
  (format stream "~
Name:~12t~a
Status:~12t~:[Enabled~;Disabled~]
Version:~12t~a
Sources:~12t~a
Directory:~12t~a
Versions:~12t~a~%"
          (name project)
          (disabled-p project)
          (version project)
          (mapcar #'serialize (sources project))
          (namestring (source-directory project))
          (mapcar #'version (releases project))))

(defmethod (setf releases) :around (releases (project project))
  (call-next-method (sort (loop for release in releases collect (ensure-release release project)) #'version>) project))

(defmethod (setf sources) :around (sources (project project))
  (call-next-method (loop for source in sources collect (ensure-source source)) project))

(defmethod ensure-release ((spec cons) (project project))
  (destructuring-bind (version . initargs) spec
    (apply #'ensure-instance (find-release version project) 'project-release
           :project project :version version initargs)))

(defmethod make-release ((project project) &key update version verbose)
  (when verbose
    (verbose "Processing ~a" (name project)))
  (when (or update version)
    (update project :version version :verbose verbose))
  (let ((version (version project)))
    (or (find-release version project)
        (make-instance 'project-release
                       :project project
                       :version version))))

(defmethod source-files ((project project))
  (gather-sources (source-directory project) (append (excluded-paths project)
                                                     *excluded-paths*)))

(defmethod systems ((project project))
  (loop for asd in (loop for file in (source-files project)
                         when (string= "asd" (pathname-type file))
                         collect file)
        append (loop for (name file deps) in (find-file-systems asd)
                     unless (find name (excluded-systems project) :test #'string-equal)
                     collect (make-instance 'system :project project :name name :file file :dependencies deps))))

(defmethod dists ((project project))
  (loop for dist in (list-dists)
        when (and (releases dist)
                  (find project (projects (first (releases dist))) :key #'project))
        collect dist))

(defmethod find-release (version (project project))
  (find version (releases project) :key #'version :test #'equal))

(defmethod ensure-project ((project project))
  project)

(defmethod remove-project ((project project) (dist dist))
  (setf (projects dist) (remove project (projects dist))))

(defmethod remove-project (project (dist dist))
  (remove-project (find-project project dist) dist))

(defmethod remove-project (project (dist symbol))
  (remove-project project (or (dist dist) (error "No dist with name ~s found." dist))))

(defmethod add-project ((project project) (dist dist))
  (let ((prior (find (name project) (projects dist) :key #'name :test #'equalp)))
    (when prior
      (cerror "Replace the existing project" "A project with name ~a already exists on ~a:~%  ~a"
              (name project) dist prior)
      (setf (projects dist) (remove prior (projects dist)))))
  (push project (projects dist))
  project)

(defmethod add-project ((pathname pathname) (dist dist))
  (let ((name (car (last (pathname-directory pathname)))))
    (add-project (make-instance 'project :name name :source-directory pathname) dist)))

(defmethod add-project (project (dist symbol))
  (add-project project (or (dist dist) (error "No dist with name ~s found." dist))))

(defmethod add-project (thingy (dist dist))
  (add-project (ensure-project thingy) dist))

(defmethod checkout ((project project) path &rest args &key (version (version project)) &allow-other-keys)
  (ensure-directories-exist path)
  (simple-inferiors:with-chdir ((source-directory project))
    (loop for source in (sources project)
          do (restart-case (return (apply #'checkout source path :version version args))
               (continue ()
                 :report "Try the next source."))
          finally (error "No capable source to clone~%  ~a" project))))

(defmethod update ((project project) &rest args &key version &allow-other-keys)
  (when (or (null version)
            (not (equal (version project) version)))
    (simple-inferiors:with-chdir ((source-directory project))
      (setf (version-cache project) NIL)
      (loop for source in (sources project)
            do (restart-case (return (apply #'update source args))
                 (continue ()
                   :report "Try the next source."))
            finally (cerror "Ignore the update failure." "No capable source to update~%  ~a"
                            project))
      (let ((release (find-release (version project) project)))
        (when release
          (setf (source-files release) T)))
      project)))

(defmethod clone ((project project) &rest args &key &allow-other-keys)
  (ensure-directories-exist (source-directory project))
  (simple-inferiors:with-chdir ((source-directory project))
    (setf (version-cache project) NIL)
    (loop for source in (sources project)
          do (restart-case (return (apply #'clone source args))
               (continue ()
                 :report "Try the next source."))
          finally (error "No capable source to clone~%  ~a" project))))

(defmethod version ((project project))
  (or (version-cache project)
      (setf (version-cache project)
            (simple-inferiors:with-chdir ((source-directory project))
              (loop for source in (sources project)
                    thereis (ignore-errors (version source)))))
      (when (releases project)
        (version (first (releases project))))))

(defmethod list-versions ((project project))
  (mapcar #'version (releases project)))

(defmethod find-system (name (project project))
  (when (releases project)
    (find-system name (first (releases project)))))

(defmethod url ((project project))
  (format NIL "/~a" (pathname-utils:unix-namestring (path project))))

(defmethod index-url ((project project))
  (format NIL "/~a" (pathname-utils:unix-namestring (path project))))

(defmethod path ((project project))
  (make-pathname :directory `(:relative "archives" ,(name project))))

(defmacro define-project (name sources &body body)
  (form-fiddle:with-body-options (releases initargs) body
    (let ((name (string-downcase name)))
      `(let ((project (setf (project ,name)
                            (ensure-instance (project ',name) 'project :name ,name :sources ',sources
                                             ,@(loop for (k v) on initargs by #'cddr
                                                     collect k collect `',v)))))
         ,@(loop for release in releases
                 collect `(ensure-release ',release project))
         project))))

(defun quick-add-projects (source-type &rest urls)
  (loop for url in urls
        for name = (url-extract-name url)
        collect (setf (project name) (ensure-instance (project name) 'project :name name :sources `((,source-type ,url))))))

(defmethod retrieve-all :after ((storage storage) (object project))
  (dolist (object (releases object))
    (retrieve-all storage object)))
