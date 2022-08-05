#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.redist)

(defvar *dists* (make-hash-table :test 'eql))
(defvar *projects* (make-hash-table :test 'equalp))

(defmethod dist ((name symbol))
  (gethash name *dists*))

(defmethod (setf dist) ((dist dist) (name symbol))
  (setf (gethash name *dists*) value))

(defmethod project ((name string))
  (gethash name *projects*))

(defmethod (setf project) ((project project) (name string))
  (setf (gethash name *projects*) project))

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

(defun persist (&key (file #p "~/dist/distinfo.lisp") (if-exists :supersede))
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

(defun restore (&key (file #p "~/dist/distinfo.lisp") (if-does-not-exist :error))
  (with-standard-io-syntax
    (let ((*package* #.*package*))
      (load file :if-does-not-exist if-does-not-exist))))
