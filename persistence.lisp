#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.qshirakumo.redist)

(defvar *dists* (make-hash-table :test 'eql))

(defmethod dist ((name symbol))
  (gethash name *dists*))

(defmethod (setf dist) (value (name symbol))
  (setf (gethash name *dists*) value))

(defgeneric serialize (thing)
  (:method-combination append :most-specific-last))

(defmethod serialize append ((dist dist))
  (list :type (type-of dist)
        :url (url dist)
        :excluded-paths (excluded-paths dist)))

(defmethod serialize :around ((dist dist))
  `(define-dist ,(name dist) ,(call-next-method)
     ,@(mapcar #'serialize (projects dist))
     ,@(mapcar #'serialize (releases dist))))

(defmethod serialize append ((project project))
  (list* (name project) (mapcar #'serialize (sources project))
         (prune-plist
          (list :active-p (active-p project)
                :source-directory (source-directory project)
                :excluded-systems (excluded-systems project)
                :excluded-paths (excluded-paths project)))))

(defmethod serialize append ((release release))
  (list :release (version release)
        :projects (mapcar #'serialize (projects release))))

(defmethod serialize append ((release project-release))
  ;; FIXME: persisting instances that are inherited from prior releases
  (list (name (project release))
        :release (version (release release))
        :version (version release)
        :source-files (source-files release)
        :systems (mapcar #'serialize (systems release))))

(defmethod serialize append ((system system))
  (list (name system)
        :file (file system)
        :dependencies (dependencies system)))

(defmethod serialize append ((manager source-manager))
  (list (type-of manager) (url manager)))

(defun persist (&key (dist T) (file #p "~/dist/") (if-exists :supersede))
  (cond ((eql T dist)
         (loop for dist being the hash-values of *dists*
               do (persist :dist dist :file (make-pathname :name (string-downcase (name dist)) :type "distinfo" :defaults file) :if-exists if-exists)))
        (T
         (ensure-directories-exist file)
         (with-open-file (stream file :direction :output :if-exists if-exists)
           (with-standard-io-syntax
             (let ((*package* #.*package*)
                   (*print-case* :downcase)
                   (*print-right-margin* 80))
               (pprint '(in-package #.(package-name *package*)) stream)
               (terpri stream)
               (pprint (serialize dist) stream)
               (terpri stream)))))))

(defun restore (&key (file #p "~/dist/*.distinfo") (if-does-not-exist :error))
  (if (wild-pathname-p file)
      (dolist (path (directory file))
        (restore :file path))
      (with-open-file (stream file :direction :input :if-does-not-exist if-does-not-exist)
        (when stream
          (with-standard-io-syntax
            (let ((*package* #.*package*))
              (loop for form = (read stream NIL #1='#:eof)
                    until (eq form #1#)
                    do (eval form))))))))

(defmacro define-dist (name initargs &body body)
  (let ((projects (loop for thing in body
                        unless (eql :release (car thing))
                        collect thing))
        (releases (loop for thing in body
                        when (eql :release (car thing))
                        collect (rest thing)))
        (type (getf initargs :type)))
    (remf initargs :type)
    `(let ((existing (or (dist ',name)
                         (setf (dist ',name) (make-instance ',type :name ',name ,@(loop for (k v) on initargs by #'cddr
                                                                                        collect k collect `',v))))))
       (ensure-instance existing ',type :projects ',projects :releases ',releases))))
