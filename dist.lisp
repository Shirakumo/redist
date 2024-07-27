(in-package #:org.shirakumo.redist)

(defgeneric make-release (thing &key))
(defgeneric find-project (name dist))
(defgeneric find-release (version dist))
(defgeneric next-version (dist))

(defmethod find-system (name (all (eql T)))
  (loop for project in (list-projects)
        thereis (find-system name project)))

(defclass dist (stored-object)
  ((name :initarg :name :initform (arg! :name) :accessor name)
   (url :initarg :url :initform (arg! :url) :accessor url)
   (projects :accessor projects)
   (releases :accessor releases)
   (excluded-paths :initarg :excluded-paths :accessor excluded-paths)))

(defmethod shared-initialize :after ((dist dist) slots &key (projects NIL projects-p) (releases NIL releases-p))
  (when projects-p
    (setf (projects dist) projects))
  (when releases-p
    (setf (releases dist) releases)))

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

(defmacro define-dist (name projects &body body)
  (form-fiddle:with-body-options (releases initargs) body
    (let ((type (getf initargs :type 'timestamp-versioned-dist)))
      (remf initargs :type)
      `(let ((dist (setf (dist ',name)
                         (ensure-instance (dist ',name) ',type
                                          :name ',name ,@(loop for (k v) on initargs by #'cddr
                                                               collect k collect `',v)
                                          :projects ',projects :releases ',releases))))
         dist))))

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
