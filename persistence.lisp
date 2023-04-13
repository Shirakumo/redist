#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.redist)

(defvar *distinfo-file* NIL)
(defvar *dists* (make-hash-table :test 'equalp))
(defvar *projects* (make-hash-table :test 'equalp))

(defun clear ()
  (clrhash *dists*)
  (clrhash *projects*))

(defun distinfo-file ()
  (or *distinfo-file*
      (when *default-source-directory*
        (merge-pathnames "../distinfo.lisp" *default-source-directory*))
      (when *default-output-directory*
        (merge-pathnames "../distinfo.lisp" *default-output-directory*))
      (merge-pathnames "dist/distinfo.lisp" (user-homedir-pathname))))

(defmethod dist ((name symbol))
  (dist (string name)))

(defmethod dist ((name string))
  (gethash name *dists*))

(defmethod (setf dist) ((dist dist) (name symbol))
  (setf (dist (string name)) dist))

(defmethod (setf dist) ((dist dist) (name string))
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

(defun quick-add-projects (source-type &rest urls)
  (loop for url in urls
        for name = (url-extract-name url)
        collect (setf (project name) (ensure-instance (project name) 'project :name name :sources `((,source-type ,url))))))

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
        :source-directory (pathname-utils:enough-pathname
                           (source-directory project) (default-source-directory))
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
        ;; NOTE: we are omitting the source-files here as they're not necessary for dist metadata
        ;;       and would have to be re-gathered from checkout on a forced recompile anyway.
        :source-sha1 (source-sha1 release)
        :archive-md5 (archive-md5 release)
        :systems (mapcar #'serialize (systems release))))

(defmethod serialize append ((system system))
  (list (name system)
        :file (enough-namestring (file system) (source-directory (project (project system))))
        :dependencies (dependencies system)))

(defmethod serialize append ((manager source-manager))
  (list (type-of manager) (url manager)))

(defun persist (&key (file (distinfo-file)) (if-exists :supersede))
  (ensure-directories-exist file)
  (with-open-file (stream file :direction :output :if-exists if-exists)
    (with-standard-io-syntax
      (let ((*package* #.*package*)
            (*print-case* :downcase)
            (*print-right-margin* 80)
            (*print-readably* NIL))
        (format stream "~&;;;;; Distinfo compiled automatically")
        (pprint '(in-package #.(package-name *package*)) stream)
        (terpri stream)
        (format stream "~&~%;;;; Projects")
        (loop for project being the hash-values of *projects*
              do (pprint (serialize project) stream))
        (format stream "~&~%;;;; Dists")
        (loop for dist being the hash-values of *dists*
              do (pprint (serialize dist) stream))
        (terpri stream)))))

(defun restore (&key (file (distinfo-file)) (if-does-not-exist :error))
  (with-standard-io-syntax
    (let ((*package* #.*package*))
      (load file :if-does-not-exist if-does-not-exist))))
