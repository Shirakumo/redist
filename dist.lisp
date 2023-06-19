#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.redist)

(defvar *default-source-directory* NIL)
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

(defun default-source-directory ()
  (or *default-source-directory*
      (make-pathname :name NIL :type NIL :defaults (merge-pathnames "sources/" (distinfo-file)))))

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

(defmethod find-system (name (all (eql T)))
  (loop for project in (list-projects)
        thereis (find-system name project)))

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

(defmethod describe-object ((dist dist) stream)
  (format stream "~
Name:~12t~a
Url:~12t~a
Version:~12t~a
Projects:~12t~a
Versions:~12t~a~%"
          (name dist) (url dist) (version dist)
          (mapcar #'name (projects dist))
          (mapcar #'version (releases dist))))

(defmethod (setf releases) :around ((releases cons) (dist dist))
  (call-next-method (sort (loop for release in releases collect (ensure-release release dist)) #'version>) dist))

(defmethod (setf projects) :around ((projects cons) (dist dist))
  (call-next-method (sort (loop for project in projects collect (ensure-project project)) #'string< :key #'name) dist))

(defmethod (setf projects) :around ((all (eql T)) (dist dist))
  (setf (projects dist) (loop for project being the hash-values of *projects* collect project)))

(defmethod find-release (version (dist dist))
  (find version (releases dist) :key #'version :test #'equalp))

(defmethod ensure-release (version (dist dist))
  (or (find-release version dist)
      (make-release dist :version version)))

(defmethod ensure-release ((spec cons) (dist dist))
  (destructuring-bind (version &rest args) spec
    (apply #'ensure-instance
           (find-release version dist) 'release
           :dist dist :version version args)))

(defmethod make-release ((dist dist) &key (version (next-version dist)) update verbose (projects NIL projects-p))
  (let ((prior (find version (releases dist) :key #'version :test #'equal)))
    (when prior
      (cerror "Replace the existing release" "A release with version ~a already exists on ~a:~%  ~a"
              version dist prior)
      (setf (releases dist) (remove prior (releases dist)))))
  (let ((release (if projects-p
                     (let ((projects (loop for project in projects collect (ensure-project project))))
                       (make-instance 'release :dist dist :version version :update update :verbose verbose :projects projects))
                     (make-instance 'release :dist dist :version version :update update :verbose verbose))))
    (push release (releases dist))
    release))

(defmethod find-project ((name symbol) (dist dist))
  (find-project (string name) dist))

(defmethod find-project ((name string) (dist dist))
  (find name (projects dist) :key #'name :test #'equalp))

(defmethod ensure-project ((name string))
  (or (project name)
      (error "No project named ~s." name)))

(defmethod ensure-project ((name symbol))
  (ensure-project (string name)))

(defmethod find-system (name (dist dist))
  (loop for project in (projects dist)
        thereis (find-system name project)))

(defmethod releases-url ((dist dist))
  (format NIL "~a/~a" (url dist) (namestring (releases-path dist))))

(defmethod dist-url ((dist dist))
  (format NIL "~a/~a" (url dist) (namestring (dist-path dist))))

(defmethod path ((dist dist))
  (make-pathname :directory `(:relative ,(string-downcase (name dist)))))

(defmethod releases-path ((dist dist))
  (make-pathname :name (format NIL "~(~a~)-versions" (name dist)) :type "txt"))

(defmethod dist-path ((dist dist))
  (make-pathname :name (string-downcase (name dist)) :type "txt"))

(defmethod version ((dist dist))
  (when (releases dist)
    (version (first (releases dist)))))

(defmethod list-versions ((dist dist))
  (mapcar #'version (releases dist)))

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
    (format NIL "~4,'0d.~2,'0d.~2,'0d-~2,'0d.~2,'0d.~2,'0d" yy mm dd h m s)))

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

(defmethod update :before ((manager source-manager) &key verbose version)
  (when verbose
    (if version
        (verbose "Checking out to ~a" version)
        (verbose "Updating from ~a" (url manager)))))

(defmethod update :around ((manager source-manager) &key &allow-other-keys)
  (with-simple-restart (continue "Ignore the update and continue as if it had happened.")
    (call-next-method)))

(defmethod version ((manager source-manager))
  (digest (gather-sources simple-inferiors:*cwd*) :sha1))

(defmethod ensure-source ((source source-manager))
  source)

(defmethod ensure-source ((spec cons))
  (destructuring-bind (type url . initargs) spec
    (etypecase type
      ((or keyword string)
       (setf type (or (find-symbol (string type) #.*package*)
                      (error "No source type with name ~s found!" type))))
      (symbol))
    (apply #'make-instance type :url url initargs)))

(defclass project ()
  ((name :initarg :name :initform (arg! :name) :accessor name)
   (source-directory :accessor source-directory)
   (sources :initform NIL :accessor sources)
   (disabled-p :initarg :disabled-p :initform NIL :accessor disabled-p)
   (excluded-systems :initarg :excluded-systems :initform () :accessor excluded-systems)
   (excluded-paths :initarg :excluded-paths :initform () :accessor excluded-paths)
   (releases :initform () :accessor releases)
   (version-cache :initform NIL :accessor version-cache)))

(defmethod shared-initialize :after ((project project) slots &key (releases NIL releases-p) (sources NIL sources-p) source-directory (verbose T))
  (when source-directory (setf (source-directory project) (uiop:truenamize (absolutize source-directory (default-source-directory)))))
  (unless (slot-boundp project 'source-directory)
    (setf (source-directory project) (pathname-utils:subdirectory (default-source-directory) (name project))))
  (when releases-p (setf (releases project) releases))
  (when sources-p (setf (sources project) sources))
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
                    thereis (ignore-errors (version source)))))
      (when (releases project)
        (version (first (releases project))))))

(defmethod list-versions ((project project))
  (mapcar #'version (releases project)))

(defmethod find-system (name (project project))
  (when (releases project)
    (find-system name (first (releases project)))))

(defmethod url ((project project))
  (format NIL "/~a" (uiop:unix-namestring (path project))))

(defmethod path ((project project))
  (make-pathname :directory `(:relative "archives" ,(name project))))

(defclass release ()
  ((dist :initarg :dist :initform (arg! :dist) :accessor dist)
   (version :initarg :version :initform (arg! :version) :accessor version)
   (timestamp :initarg :timestamp :initform (get-universal-time) :accessor timestamp)
   (projects :initform () :accessor projects)))

(defmethod shared-initialize :after ((release release) slots &key (projects NIL projects-p))
  (when projects-p (setf (projects release) projects)))

(defmethod initialize-instance :after ((release release) &key dist update verbose (projects NIL projects-p))
  (declare (ignore projects))
  (unless projects-p
    (setf (projects release)
          (do-list* (project (remove-if #'disabled-p (projects dist)))
            (make-release project :update update :verbose verbose)))))

(defmethod print-object ((release release) stream)
  (print-unreadable-object (release stream :type T)
    (format stream "~a" (version release))))

(defmethod describe-object ((release release) stream)
  (format stream "~
Dist:~12t~a
Version:~12t~a
Timestamp:~12t~a
Projects:~12t~{~a ~a~^~%~12t~}~%"
          (name (dist release))
          (version release)
          (timestamp release)
          (loop for project-release in (projects release)
                collect (version project-release)
                collect (name (project project-release)))))

(defmethod (setf projects) :around (projects (release release))
  (call-next-method (loop for project in projects collect (ensure-project-release project release)) release))

(defmethod ensure-release ((release release) (dist dist))
  release)

(defmethod ensure-release ((release release) (project project))
  (make-release project :release release))

(defmethod ensure-project-release ((project project) (release release))
  (make-release project :release release))

(defmethod ensure-project-release ((spec cons) (release release))
  (destructuring-bind (project &rest initargs &key version &allow-other-keys) spec
    (let ((project (or (find-project project (dist release))
                       (error "No project named~%  ~s~%present on dist ~s!"
                              project (dist release)))))
      (remf initargs :version)
      (ensure-release (list* version initargs) project))))

(defmethod find-project ((project project) (release release))
  (find project (projects release) :key #'project))

(defmethod find-project (name (release release))
  (find name (projects release) :key #'name :test #'equalp))

(defmethod find-system (name (release release))
  (loop for project in (projects release)
        thereis (find-system name project)))

(defmethod releases-url ((release release))
  (format NIL "~a/~a" (url (dist release)) (namestring (releases-path release))))

(defmethod systems-url ((release release))
  (format NIL "~a/~a" (url (dist release)) (namestring (systems-path release))))

(defmethod dist-url ((release release))
  (format NIL "~a/~a" (url (dist release)) (namestring (dist-path release))))

(defmethod path ((release release))
  (merge-pathnames (make-pathname :directory `(:relative ,(version release)))
                   (path (dist release))))

(defmethod releases-path ((release release))
  (merge-pathnames (make-pathname :name "releases" :type "txt") (path release)))

(defmethod systems-path ((release release))
  (merge-pathnames (make-pathname :name "systems" :type "txt") (path release)))

(defmethod dist-path ((release release))
  (merge-pathnames (make-pathname :name (string-downcase (name (dist release))) :type "txt") (path release)))

(defmethod version< ((a release) (b release))
  (version< (version a) (version b)))

(defclass project-release ()
  ((project :initarg :project :initform (arg! :project) :accessor project)
   (version :initarg :version :initform (arg! :version) :accessor version)
   (systems :accessor systems)
   (source-files :initarg :source-files :accessor source-files)
   (archive-md5 :initform NIL :initarg :archive-md5 :accessor archive-md5)
   (source-sha1 :initform NIL :initarg :source-sha1 :accessor source-sha1)))

(defmethod initialize-instance :after ((release project-release) &key)
  (unless (slot-boundp release 'source-files)
    (setf (source-files release) T))
  (unless (source-sha1 release)
    (setf (source-sha1 release) (digest (source-files release) :sha1)))
  (unless (slot-boundp release 'systems)
    (setf (systems release) T))
  (pushnew release (releases (project release))))

(defmethod shared-initialize :after ((release project-release) slot &key (systems NIL systems-p))
  (when systems-p (setf (systems release) systems))
  (when (slot-boundp release 'source-files)
    (loop for cons on (source-files release)
          do (setf (car cons) (absolutize (car cons) (source-directory (project release)))))))

(defmethod (setf source-files) ((all (eql T)) (release project-release))
  (setf (source-files release) (gather-sources (source-directory (project release))
                                               (append (excluded-paths (project release))
                                                       *excluded-paths*)))
  (setf (source-sha1 release) (digest (source-files release) :sha1)))

(defmethod print-object ((release project-release) stream)
  (print-unreadable-object (release stream :type T)
    (format stream "~a ~a" (name (project release)) (version release))))

(defmethod describe-object ((release project-release) stream)
  (format stream "~
Project:~12t~a
Version:~12t~a
Archive MD5:~12t~a
Source SHA1:~12t~a
Systems:~12t~a~%"
          (name (project release))
          (version release)
          (archive-md5 release)
          (source-sha1 release)
          (mapcar #'name (systems release))))

(defmethod source-files ((release project-release))
  (let ((value (slot-value release 'source-files)))
    (or value
        (setf (source-files release)
              (gather-sources (source-directory (project release))
                              (append (excluded-paths (project release))
                                      *excluded-paths*))))))

(defmethod (setf systems) :around ((systems cons) (release project-release))
  (call-next-method (sort (loop for system in systems collect (ensure-system system release)) #'string< :key #'name) release))

(defmethod (setf systems) ((systems (eql T)) (release project-release))
  (setf (systems release)
        (loop for asd in (loop for file in (source-files release)
                               when (string= "asd" (pathname-type file))
                               collect file)
              append (loop for (name . deps) in (find-file-systems asd)
                           unless (find name (excluded-systems (project release)) :test #'string-equal)
                           collect (make-instance 'system :project release :name name :file asd :dependencies deps)))))

(defmethod ensure-system ((spec cons) (release project-release))
  (destructuring-bind (name . initargs) spec
    (apply #'make-instance 'system :project release :name name initargs)))

(defmethod ensure-project-release ((project project-release) (release release))
  project)

(defmethod ensure-release ((release project-release) (project project))
  release)

(defmethod find-system (name (release project-release))
  (loop for system in (systems release)
        thereis (find-system name system)))

(defmethod dists ((release project-release))
  (loop for dist in (list-dists)
        when (and (releases dist)
                  (find release (projects (first (releases dist)))))
        collect dist))

(defmethod name ((release project-release))
  (name (project release)))

(defmethod url ((release project-release))
  (format NIL "/~a" (uiop:unix-namestring (path release))))

(defmethod path ((release project-release))
  (make-pathname :name (format NIL "~a-~a" (name release) (version release))
                 :type "tgz" :defaults (path (project release))))

(defmethod prefix ((release project-release))
  (format NIL "~a-~a" (name release) (version release)))

(defmethod version< ((a project-release) (b project-release))
  (version< (version a) (version b)))

(defun implementation-specific-dependency-p (dep)
  (find dep '(sb-aclrepl sb-bsd-sockets sb-capstone sb-cltl2 sb-concurrency
              sb-cover sb-executable sb-gmp sb-graph sb-grovel sb-introspect
              sb-md5 sb-mpfr sb-posix sb-queue sb-rotate-byte sb-rt
              sb-simple-streams sb-sprof extensible-sequences osi unix
              syscalls winhttp package-locks sbcl-single-float-tran)
        :test #'string-equal))

(defclass system ()
  ((project :initarg :project :initform (arg! :project) :accessor project)
   (name :initarg :name :initform (arg! :name) :accessor name)
   (file :initarg :file :initform (arg! :file) :accessor file)
   (dependencies :initarg :dependencies :initform (arg! :dependencies) :accessor dependencies)))

(defmethod shared-initialize :after ((system system) slots &key (dependencies NIL dependencies-p))
  (when dependencies-p (setf (dependencies system) dependencies))
  (setf (name system) (string-downcase (name system)))
  (multiple-value-bind (absolute-p path) (pathname-utils:absolute-p (file system))
    (unless absolute-p
      (setf (file system) (merge-pathnames path (source-directory (project (project system))))))))

(defmethod (setf dependencies) :around ((dependencies cons) (system system))
  (call-next-method (delete-duplicates (sort (remove-if #'implementation-specific-dependency-p dependencies) #'string<) :test #'string=) system))

(defmethod print-object ((system system) stream)
  (print-unreadable-object (system stream :type T)
    (format stream "~a ~a" (name (project system)) (name system))))

(defmethod ensure-system ((system system) (release project-release))
  system)

(defmethod find-system (name (system system))
  (when (string-equal name (name system))
    system))
