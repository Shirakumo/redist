(in-package #:org.shirakumo.redist)

(defclass release (stored-object)
  ((dist :initarg :dist :initform (arg! :dist) :accessor dist)
   (version :initarg :version :initform (arg! :version) :accessor version)
   (timestamp :initarg :timestamp :initform (get-universal-time) :accessor timestamp)
   (projects :accessor projects)))

(defmethod shared-initialize :after ((release release) slots &key (projects NIL projects-p))
  (when (stringp (dist release))
    (setf (dist release) (or (dist (dist release))
                             (error "No dist named ~s!" (dist release)))))
  (when projects-p (setf (projects release) projects)))

(defmethod initialize-instance :after ((release release) &key dist update verbose (projects NIL projects-p))
  (declare (ignore projects))
  (when (and (not projects-p) (not (stored-p release)))
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
  (format NIL "~a/~a" (url (dist release)) (pathname-utils:unix-namestring (releases-path release))))

(defmethod systems-url ((release release))
  (format NIL "~a/~a" (url (dist release)) (pathname-utils:unix-namestring (systems-path release))))

(defmethod dist-url ((release release))
  (format NIL "~a/~a" (url (dist release)) (pathname-utils:unix-namestring (dist-path release))))

(defmethod index-url ((release release))
  (format NIL "/~a" (pathname-utils:unix-namestring (path release))))

(defmethod report-url ((release release))
  (format NIL "/~a" (pathname-utils:unix-namestring (report-path release))))

(defmethod path ((release release))
  (merge-pathnames (make-pathname :directory `(:relative ,(version release)))
                   (path (dist release))))

(defmethod report-path ((release release))
  (merge-pathnames (make-pathname :name "report" :type "html") (path release)))

(defmethod releases-path ((release release))
  (merge-pathnames (make-pathname :name "releases" :type "txt") (path release)))

(defmethod systems-path ((release release))
  (merge-pathnames (make-pathname :name "systems" :type "txt") (path release)))

(defmethod dist-path ((release release))
  (merge-pathnames (make-pathname :name (string-downcase (name (dist release))) :type "txt") (path release)))

(defmethod version< ((a release) (b release))
  (version< (version a) (version b)))

(defmethod checkout ((release release) path &rest args &key &allow-other-keys)
  (loop for project in (projects release)
        do (apply #'checkout (pathname-utils:subdirectory path (name project)) path args)))

(defmethod version-hash ((release release))
  (hash (sort (copy-seq (projects release)) #'string< :key #'name)))

(defclass project-release (stored-object)
  ((project :initarg :project :initform (arg! :project) :accessor project)
   (version :initarg :version :initform (arg! :version) :accessor version)
   (systems :accessor systems)
   (source-files :initarg :source-files :accessor source-files)
   (archive-md5 :initform NIL :initarg :archive-md5 :accessor archive-md5)
   (source-sha1 :initform NIL :initarg :source-sha1 :accessor source-sha1)))

(defmethod initialize-instance :after ((release project-release) &key)
  (unless (stored-p release)
    (unless (slot-boundp release 'source-files)
      (setf (source-files release) T))
    (unless (source-sha1 release)
      (setf (source-sha1 release) (digest (source-files release) :sha1)))
    (unless (slot-boundp release 'systems)
      (setf (systems release) T))))

(defmethod shared-initialize :after ((release project-release) slot &key (systems NIL systems-p))
  (when (stringp (project release))
    (setf (project release) (or (project (project release))
                                (error "No project named ~s!" (project release)))))
  (when systems-p
    (setf (systems release) systems))
  (unless (stored-p release)
    (if (slot-boundp release 'source-files)
        (setf (source-files release) (source-files release))
        (setf (source-files release) T))
    (unless (slot-boundp release 'systems)
      (setf (systems release) T))))

(defmethod (setf source-files) ((all (eql T)) (release project-release))
  (setf (source-files release) (gather-sources (source-directory (project release))
                                               (append (excluded-paths (project release))
                                                       *excluded-paths*)))
  (setf (source-sha1 release) (digest (source-files release) :sha1)))

(defmethod (setf source-files) :after ((files cons) (release project-release))
  (loop for cons on (source-files release)
        do (setf (car cons) (absolutize (car cons) (source-directory (project release))))))

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
  (destructuring-bind (name . initargs) (enlist spec)
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

(defmethod index-url ((release project-release))
  (format NIL "/~a" (make-pathname :name (version release) :type "html" :defaults (path release))))

(defmethod report-url ((release project-release))
  (format NIL "~a/report.html" (url release)))

(defmethod url ((release project-release))
  (format NIL "/~a" (pathname-utils:unix-namestring (path release))))

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

(defmethod checkout ((release project-release) path &rest args)
  (apply #'checkout (project release) path :version (version release) args))

(defmethod version-hash ((release project-release))
  (hash (sort (copy-seq (systems release)) #'string< :key #'name)))

(defclass system (stored-object)
  ((project :initarg :project :initform (arg! :project) :accessor project)
   (name :initarg :name :initform (arg! :name) :accessor name)
   (file :initarg :file :initform (arg! :file) :accessor file)
   (dependencies :initarg :dependencies :initform (arg! :dependencies) :accessor dependencies)))

(defmethod shared-initialize :after ((system system) slots &key (dependencies NIL dependencies-p))
  (when dependencies-p
    (setf (dependencies system) dependencies))
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

(defmethod version ((system system))
  (version (project system)))

(defmethod safe-name ((system system))
  (map 'string (lambda (c) (if (find c "/\\|*\":;?<>") #\- c)) (name system)))

(defmethod version-hash ((system system))
  (let ((chain (list (list (name system) (version system)))))
    ;; FIXME: we need to hash within the environment the system is in...
    (dolist (dependency (dependencies system))
      (let ((proj (or (find-system dependency system)
                      (find-system dependency (project system))
                      (find-system dependency T))))
        (when proj
          (push (list dependency (version proj)) chain))))
    (hash (sort chain #'string< :key #'car))))

(defmethod report-url ((system system))
  (format NIL "/~a/test/~a.html" (url (project system)) (safe-name system)))
