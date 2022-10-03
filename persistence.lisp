#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.redist)

(defvar *distinfo-file* (merge-pathnames "../distinfo.lisp" *default-source-directory*))
(defvar *dists* (make-hash-table :test 'eql))
(defvar *projects* (make-hash-table :test 'equalp))

(defmethod dist ((name symbol))
  (gethash name *dists*))

(defmethod (setf dist) ((dist dist) (name symbol))
  (setf (gethash name *dists*) dist))

(defun list-dists ()
  (sort (alexandria:hash-table-values *dists*) #'string< :key #'name))

(defmethod project ((name string))
  (gethash name *projects*))

(defmethod (setf project) ((project project) (name string))
  (setf (gethash name *projects*) project))

(defun list-projects ()
  (sort (alexandria:hash-table-values *projects*) #'string< :key #'name))

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

(defgeneric serialize (thing)
  (:method-combination append :most-specific-last))

(defmethod serialize :around ((dist dist))
  `(define-dist ,(name dist) ,(mapcar #'name (projects dist))
     ,@(prune-plist (call-next-method))
     ,@(mapcar #'serialize (releases dist))))

(defmethod serialize append ((dist dist))
  (list :type (type-of dist)
        :url (url dist)
        :excluded-paths (excluded-paths dist)))

(defmethod serialize :around ((project project))
  `(define-project ,(name project)
       ,(mapcar #'serialize (sources project))
     ,@(prune-plist (call-next-method))
     ,@(mapcar #'serialize (releases project))))

(defmethod serialize append ((project project))
  (list :disabled-p (disabled-p project)
        :source-directory (source-directory project)
        :excluded-systems (excluded-systems project)
        :excluded-paths (excluded-paths project)
        :releases (mapcar #'serialize (releases project))))

(defmethod serialize append ((release release))
  (list (version release)
        :timestamp (timestamp release)
        :projects (loop for project in (projects release)
                        collect (list (name project) :version (version project)))))

(defmethod serialize append ((release project-release))
  (list (version release)
        :source-files (source-files release)
        :source-sha1 (source-sha1 release)
        :archive-md5 (archive-md5 release)
        :systems (mapcar #'serialize (systems release))))

(defmethod serialize append ((system system))
  (list (name system)
        :file (file system)
        :dependencies (dependencies system)))

(defmethod serialize append ((manager source-manager))
  (list (type-of manager) (url manager)))

(defun persist (&key (file *distinfo-file*) (if-exists :supersede))
  (ensure-directories-exist file)
  (with-open-file (stream file :direction :output :if-exists if-exists)
    (with-standard-io-syntax
      (let ((*package* #.*package*)
            (*print-case* :downcase)
            (*print-right-margin* 80)
            (*print-readably* NIL))
        (format stream "~&;;;;; Distinfo compiled automatically~%")
        (pprint '(in-package #.(package-name *package*)) stream)
        (terpri stream)
        (format stream "~&;;;; Projects~%")
        (loop for project being the hash-values of *projects*
              do (pprint (serialize project) stream))
        (format stream "~&;;;; Dists~%")
        (loop for dist being the hash-values of *dists*
              do (pprint (serialize dist) stream))
        (terpri stream)))))

(defun restore (&key (file *distinfo-file*) (if-does-not-exist :error))
  (with-standard-io-syntax
    (let ((*package* #.*package*))
      (load file :if-does-not-exist if-does-not-exist))))

(defun create-update-script (&key (file (merge-pathnames "redist.lisp" *distinfo-file*)) (if-exists :supersede))
  (ensure-directories-exist file)
  (with-open-file (stream file :direction :output :if-exists if-exists)
    (with-standard-io-syntax
      (let ((*package* #.*package*)
            (*print-case* :downcase)
            (*print-right-margin* 80)
            (*print-readably* NIL))
        (format stream "~&#| Dist update script compiled automatically~%")
        (format stream "~&~a --load \"$0\" -- \"$@\"; exit~%" (first (uiop:raw-command-line-arguments)))
        (format stream "~&# |#~%~%")
        (format stream "~&#-quicklisp (load ~s)~%" (ql-impl-util::quicklisp-init-file-form))
        (format stream "~&(ql:quickload :redist :silent T)~%~%")
        (format stream "~&(setf org.shirakumo.redist:*distinfo-file* ~s)~%" *distinfo-file*)
        (format stream "~&(org.shirakumo.redist:main (rest (uiop:command-line-arguments)))~%")))))

(defun main (&optional (args (uiop:command-line-arguments)))
  (let ((args (or args '("--help")))
        (dists '())
        (update NIL)
        (compile NIL)
        (verbose NIL)
        (version NIL))
    (loop for (key val) = args
          while key
          do (flet ((argp (&rest choices)
                      (loop for choice in choices thereis (string-equal key choice))))
               (cond ((argp "--dist" "-d")
                      (push val dists)
                      (setf args (cddr args)))
                     ((argp "--info" "-i")
                      (setf *distinfo-file* (pathname-utils:parse-native-namestring val))
                      (setf args (cddr args)))
                     ((argp "--version")
                      (setf version val)
                      (setf args (cddr args)))
                     ((argp "--update" "-u")
                      (setf update T)
                      (setf args (cdr args)))
                     ((argp "--compile" "-c")
                      (setf compile T)
                      (setf args (cdr args)))
                     ((argp "--verbose" "-v")
                      (setf verbose T)
                      (setf args (cdr args)))
                     ((argp "--list-dists" "-l")
                      (dolist (dist (list-dists))
                        (format T "~&~a~%" dist))
                      (uiop:quit))
                     ((argp "--help" "-h")
                      (format T "Usage: ~a

Arguments:
  [-d|--dist dist]*          The dists to tackle. Can be specified many times.
  [-i|--info distinfo-file]  The distinfo file to load from.
  [--version version]        The version to produce
  [--update|-u]              Updates dists when specified
  [--compile|-c]             Compiles dists when specified
  [--verbose|-v]             Produces verbose output when specified
  [--list-dists|-l]          Lists known dists when specified and quits
  [--help|-h]                Shows this help when specified and quits"
                              (first (uiop:raw-command-line-arguments)))
                      (uiop:quit))
                     (T
                      (format T "~&Unknown argument: ~a" key)
                      (uiop:quit 1)))))
    (unless dists
      (setf dists (list-dists)))
    (when update
      (dolist (dist dists)
        (update dist :version version :verbose verbose))
      (persist))
    (when compile
      (dolist (dist dists)
        (compile dist :if-exists :supersede :version version :verbose verbose))
      (persist))
    (uiop:quit)))
