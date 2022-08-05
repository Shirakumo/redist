#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
                           #p".travis.yml"))

(defgeneric make-release (thing &key))
(defgeneric find-project (name dist))
(defgeneric find-release (version dist))
(defgeneric next-version (dist))

(defun arg! (initarg)
  (error "~s required." initarg))

(defgeneric version< (a b)
  (:method ((a real) (b real)) (< a b))
  (:method ((a string) (b string)) (string< a b)))

(defun version> (a b)
  (version< b a))

(defclass dist ()
  ((name :initarg :name :initform (arg! :name) :accessor name)
   (url :initarg :url :initform (arg! :url) :accessor url)
   (projects :initform () :accessor projects)
   (releases :initform () :accessor releases)
   (excluded-paths :initarg :excluded-paths :initform () :accessor excluded-paths)))

(defmethod shared-initialize :after ((dist dist) slots &key (projects NIL projects-p) (releases NIL releases-p))
  (when projects-p (setf (projects dist) projects))
  (when releases-p (setf (releases dist) releases)))

(defmethod print-object ((dist dist) stream)
  (print-unreadable-object (dist stream :type T)
    (format stream "~s ~a" (name dist) (url dist))))

(defmethod (setf releases) :around (releases (dist dist))
  (call-next-method (sort (loop for release in releases collect (ensure-release release dist)) #'version>) dist))

(defmethod (setf projects) :around (projects (dist dist))
  (let ((new (loop for project in projects collect (ensure-project project dist))))
    ;; Ensure we can't remove projects, only disable.
    (dolist (project (projects dist))
      (unless (find project new)
        (setf (active-p project) NIL)
        (push project new)))
    (call-next-method new dist)))

(defmethod find-release (version (dist dist))
  (find version (releases dist) :key #'version :test #'equalp))

(defmethod ensure-release (version (dist dist))
  (or (find-release version dist)
      (make-release dist :version version)))

(defmethod ensure-release ((spec cons) (dist dist))
  (destructuring-bind (version &rest args) spec
    (apply #'ensure-instance
           (find-release version dist) 'release
           (list* :version version args))))

(defmethod make-release ((dist dist) &key (version (next-version dist)) update verbose (projects NIL projects-p))
  (let ((prior (find version (releases dist) :key #'version :test #'equal)))
    (when prior
      (cerror "Replace the existing release" "A release with version ~a already exists on ~a:~%  ~a"
              version dist prior)
      (setf (releases dist) (remove prior (releases dist)))))
  (let ((release (if projects-p
                     (let ((projects (loop for project in projects collect (ensure-project project dist))))
                       (make-instance 'release :dist dist :version version :update update :verbose verbose :projects projects))
                     (make-instance 'release :dist dist :version version :update update :verbose verbose))))
    (push release (releases dist))
    release))

(defmethod find-project ((name symbol) (dist dist))
  (find-project (string name) dist))

(defmethod find-project ((name string) (dist dist))
  (find name (projects dist) :key #'name :test #'equalp))

(defmethod ensure-project ((pathname pathname) (dist dist))
  (let ((name (car (last (pathname-directory pathname)))))
    (ensure-project (make-instance 'project :name name :source-directory pathname) dist)))

(defmethod ensure-project ((name symbol) (dist dist))
  (or (find-project name dist)
      (error "No project named ~s on ~a" name dist)))

(defmethod ensure-project ((name string) (dist dist))
  (or (find-project name dist)
      (cond ((string= "" name)
             (error "Can't use empty project names."))
            ((char= #\/ (char name 0))
             (ensure-project (pathname name) dist))
            (T
             (loop for hook in *string-type-parse-hooks*
                   for type = (funcall hook name)
                   do (when type (return (ensure-project (list type name) dist)))
                   finally (error "Don't know how to turn~%  ~s~%into a project on ~a."
                                  name dist))))))

(defmethod ensure-project ((spec cons) (dist dist))
  (destructuring-bind (name &optional sources &rest args) spec
    (unless (listp (first sources)) (setf sources (list sources)))
    (apply #'ensure-instance
           (find-project name dist) 'project
           (list* :name name
                  :sources (loop for (type-ish url . args) in sources
                                 for type = (etypecase type
                                              (keyword (intern (string type-ish) #.*package*))
                                              (symbol type-ish)
                                              (source-manager type-ish))
                                 collect (if (typep type 'source-manager)
                                             type
                                             (apply #'make-instance type :url url args)))
                  args))))

(defmethod releases-url ((dist dist))
  (format NIL "~a/~a" (url dist) (namestring (releases-path dist))))

(defmethod dist-url ((dist dist))
  (format NIL "~a/~a" (url dist) (namestring (dist-path dist))))

(defmethod releases-path ((dist dist))
  (make-pathname :name (format NIL "~(~a~)-versions" (name dist)) :type "txt"))

(defmethod dist-path ((dist dist))
  (make-pathname :name (string-downcase (name dist)) :type "txt"))

(defclass integer-versioned-dist (dist)
  ())

(defmethod next-version ((dist integer-versioned-dist))
  (if (releases dist)
      (1+ (version (car (last (releases dist)))))
      1))

(defclass timestamp-versioned-dist (dist)
  ())

(defmethod next-version ((dist timestamp-versioned-dist))
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time (get-universal-time) 0)
    (format NIL "~4,'0d.~2,'0d.~2,'0d-~2,'0d:~2,'0d:~2,'0d" yy mm dd h m s)))

(defclass date-versioned-dist (dist)
  ())

(defmethod next-version ((dist date-versioned-dist))
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time (get-universal-time) 0)
    (declare (ignore s m h))
    (format NIL "~4,'0d-~2,'0d-~2,'0d" yy mm dd)))

(defclass source-manager ()
  ((url :initarg :url :initform (arg! :url) :accessor url)))

(defmethod print-object ((manager source-manager) stream)
  (print-unreadable-object (manager stream :type T)
    (format stream "~a" (url manager))))

(defgeneric version (manager))
(defgeneric update (manager &key))
(defgeneric clone (manager &key))

(defmethod clone :before ((manager source-manager) &key verbose)
  (when verbose
    (verbose "Cloning from ~a ~a" (type-of manager) (url manager))))

(defmethod update :before ((manager source-manager) &key verbose)
  (when verbose
    (verbose "Updating from ~a" (url manager))))

(defmethod update :around ((manager source-manager) &key &allow-other-keys)
  (with-simple-restart (continue "Ignore the update and continue as if it had happened.")
    (call-next-method)))

(defmethod version ((manager source-manager))
  (digest (gather-sources simple-inferiors:*cwd*) :sha1))

(defclass project ()
  ((name :initarg :name :initform (arg! :name) :accessor name)
   (source-directory :initarg :source-directory :initform (arg! :source-directory) :accessor source-directory)
   (sources :initarg :sources :initform NIL :accessor sources)
   (active-p :initarg :active-p :initform T :accessor active-p)
   (excluded-systems :initarg :excluded-systems :initform () :accessor excluded-systems)
   (excluded-paths :initarg :excluded-paths :initform () :accessor excluded-paths)
   (releases :initarg :releases :initform () :accessor releases)
   (version-cache :initform NIL :accessor version-cache)))

(defmethod shared-initialize :after ((project project) slots &key (releases NIL releases-p))
  (when releases-p (setf (releases dist) releases))
  (when (and (sources project)
             (active-p project)
             (or (not (probe-file (source-directory project)))
                 (null (directory (merge-pathnames pathname-utils:*wild-path* (source-directory project))))))
    (restart-case
        (clone project :verbose T)
      (deactivate ()
        :report "Deactivate the project"
        (setf (active-p project) NIL)))))

(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type T)
    (format stream "~a ~:[INACTIVE~;ACTIVE~]" (name project) (active-p project))))

(defmethod (setf releases) :around (releases (project project))
  (call-next-method (sort (loop for release in releases collect (ensure-release release project)) #'version>) project))

(defmethod make-release ((project project) &key release update version verbose)
  (or (find release (releases project) :key #'release)
      (progn
        (when verbose
          (verbose "Processing ~a" (name project)))
        (when (or update version)
          (update project :version version :verbose verbose))
        (let ((version (version project)))
          (or (find-release version project)
              (make-instance 'project-release
                             :project project
                             :release release
                             :version version))))))

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

(defmethod find-release (version (project project))
  (find version (releases project) :key #'version :test #'equal))

(defmethod ensure-project ((project project) dist)
  project)

(defmethod remove-project ((project project) (dist dist))
  (setf (active-p project) NIL))

(defmethod remove-project (project (dist dist))
  (remove-project (find-project project dist) dist))

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

(defmethod add-project (thingy (dist dist))
  (add-project (ensure-project thingy dist) dist))

(defmethod update ((project project) &rest args &key &allow-other-keys)
  (simple-inferiors:with-chdir ((source-directory project))
    (setf (version-cache project) NIL)
    (loop for source in (sources project)
          do (restart-case (return (apply #'update source args))
               (continue ()
                 :report "Try the next source."))
          finally (cerror "Ignore the update failure." "No capable source to update~%  ~a"
                          project))))

(defmethod clone ((project project) &rest args &key &allow-other-keys)
  (simple-inferiors:with-chdir ((source-directory project))
    (setf (version-cache project) NIL)
    (ensure-directories-exist (source-directory project))
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
                    thereis (ignore-errors (version source)))))))

(defclass release ()
  ((dist :initarg :dist :initform (arg! :dist) :accessor dist)
   (version :initarg :version :initform (arg! :version) :accessor version)
   (projects :accessor projects)))

(defmethod shared-initialize :after ((release release) slots &key (projects NIL projects-p))
  (when projects-p (setf (projects release) projects)))

(defmethod initialize-instance :after ((release release) &key dist update verbose)
  (unless (slot-boundp release 'projects)
    (setf (projects release)
          (loop for project in (projects dist)
                when (active-p project)
                collect (make-release project :release release :update update :verbose verbose)))))

(defmethod print-object ((release release) stream)
  (print-unreadable-object (release stream :type T)
    (format stream "~a" (version release))))

(defmethod (setf projects) :around (projects (release release))
  (call-next-method (loop for project in projects collect (ensure-project-release project release)) release))

(defmethod ensure-release ((release release) (dist dist))
  release)

(defmethod ensure-release ((release release) (project project))
  (make-release project :release release))

(defmethod ensure-project-release ((project project) (release release))
  (make-release project :release release))

(defmethod ensure-project-release ((spec cons) (release release))
  (destructuring-bind (project &rest initargs) spec
    (let ((project (or (find-project project (dist release))
                       (error "No project named~%  ~s~%present on dist ~s!"
                              project (dist release))))
          (systems (getf initargs :systems))
          (named-release (getf initargs :release)))
      (remf initargs :systems)
      (remf initargs :release)
      (if (equal named-release (version release))
          (ensure-instance (find-project project release)
                           'project-release
                           (list* :project project
                                  :release release
                                  :systems (loop for (name . args) in systems
                                                 collect (apply #'make-instance 'system :project project :name name args))
                                  initargs))
          (ensure-project-release (list* :systems systems spec)
                                  (ensure-release named-release (dist release)))))))

(defmethod find-project ((project project) (release release))
  (find project (projects release) :key #'project))

(defmethod find-project (name (release release))
  (find name (projects release) :key #'name :test #'equalp))

(defmethod releases-url ((release release))
  (format NIL "~a/~a" (url (dist release)) (namestring (releases-path release))))

(defmethod systems-url ((release release))
  (format NIL "~a/~a" (url (dist release)) (namestring (systems-path release))))

(defmethod dist-url ((release release))
  (format NIL "~a/~a" (url (dist release)) (namestring (dist-path release))))

(defmethod releases-path ((release release))
  (make-pathname :name "releases" :type "txt" :directory `(:relative ,(version release))))

(defmethod systems-path ((release release))
  (make-pathname :name "systems" :type "txt" :directory `(:relative ,(version release))))

(defmethod dist-path ((release release))
  (make-pathname :name (string-downcase (name (dist release))) :type "txt" :directory `(:relative ,(version release))))

(defmethod version< ((a release) (b release))
  (version< (version a) (version b)))

(defclass project-release ()
  ((project :initarg :project :initform (arg! :project) :accessor project)
   (release :initarg :release :initform (arg! :release) :accessor release)
   (version :initarg :version :initform (arg! :version) :accessor version)
   (systems :initarg :systems :accessor systems)
   (source-files :initarg :source-files :accessor source-files)
   (archive-md5 :initform NIL :initarg :archive-md5 :accessor archive-md5)
   (source-sha1 :initform NIL :initarg :source-sha1 :accessor source-sha1)))

(defmethod initialize-instance :after ((release project-release) &key)
  (unless (slot-boundp release 'source-files)
    (setf (source-files release) (gather-sources (source-directory (project release))
                                                 (append (excluded-paths (project release))
                                                         (excluded-paths (dist release))
                                                         *excluded-paths*))))
  (unless (source-sha1 release)
    (setf (source-sha1 release) (digest (source-files release) :sha1)))
  (unless (slot-boundp release 'systems)
    (setf (systems release)
          (loop for asd in (loop for file in (source-files release)
                                 when (string= "asd" (pathname-type file))
                                 collect file)
                append (loop for (name . deps) in (find-file-systems asd)
                             unless (find name (excluded-systems (project release)) :test #'string-equal)
                             collect (make-instance 'system :project release :name name :file asd :dependencies deps)))))
  (pushnew release (releases (project release))))

(defmethod print-object ((release project-release) stream)
  (print-unreadable-object (release stream :type T)
    (format stream "~a ~a" (name (project release)) (version (release release)))))

(defmethod ensure-project-release ((project project-release) (release release))
  project)

(defmethod ensure-release ((release project-release) (project project))
  release)

(defmethod dist ((release project-release))
  (dist (release release)))

(defmethod name ((release project-release))
  (name (project release)))

(defmethod url ((release project-release))
  (format NIL "~a/~a" (url (dist release)) (uiop:unix-namestring (path release))))

(defmethod path ((release project-release))
  (make-pathname :name (format NIL "~a-~a" (name release) (version release))
                 :type "tgz" :directory `(:relative "archives" ,(name release))))

(defmethod prefix ((release project-release))
  (format NIL "~a-~a" (name release) (version release)))

(defmethod version< ((a project-release) (b project-release))
  (version< (version a) (version b)))

(defclass system ()
  ((project :initarg :project :initform (arg! :project) :accessor project)
   (name :initarg :name :initform (arg! :name) :accessor name)
   (file :initarg :file :initform (arg! :file) :accessor file)
   (dependencies :initarg :dependencies :initform (arg! :dependencies) :accessor dependencies)))

(defmethod print-object ((system system) stream)
  (print-unreadable-object (system stream :type T)
    (format stream "~a ~a" (name (project system)) (name system))))
