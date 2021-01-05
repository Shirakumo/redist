#|
 This file is a part of Shirakumo-Dist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defgeneric make-release (thing &key))

(defun arg! (initarg)
  (error "~s required." initarg))

(defclass dist ()
  ((name :initarg :name :initform (arg! :dist) :accessor name)
   (projects :initarg :projects :initform (arg! :dist) :accessor projects)
   (url :initarg :url :initform (arg! :url) :accessor url)))

(defmethod make-release ((dist dist) &key (version (next-version dist)))
  (make-instance 'release
                 :dist dist
                 :version version))

(defclass project ()
  ((name :initarg :name :initform (arg! :name) :accessor name)
   (source-directory :initarg :source-directory :initform (arg! :source-directory) :accessor source-directory)))

(defmethod make-release ((project project) &key release)
  (make-instance 'project-release
                 :project project
                 :release release))

(defmethod source-files ((project project))
  (directory (merge-pathnames (make-pathname :name :wild :type :wild :directory '(:relative :wild-inferiors))
                              (source-directory project))))

(defclass release ()
  ((dist :initarg :dist :initform (arg! :dist) :accessor dist)
   (version :initarg :version :initform (next-version) :accessor version)
   (projects :initarg :projects :accessor projects)))

(defmethod initialize-instance :after ((release release) &key dist version)
  (unless (slot-boundp release 'projects)
    (setf (projects release)
          (loop for project in (projects dist)
                collect (make-release project :release release)))))

(defmethod releases-url ((release release))
  (format NIL "~a/~a" (url (dist release)) (namestring (releases-path release))))

(defmethod systems-url ((release release))
  (format NIL "~a/~a" (url (dist release)) (namestring (systems-path release))))

(defmethod releases-path ((release release))
  (make-pathname :name "releases" :type "txt" :directory `(:relative ,(version path))))

(defmethod systems-path ((release release))
  (make-pathname :name "systems" :type "txt" :directory `(:relative ,(version path))))

(defclass project-release ()
  ((project :initarg :project :initform (arg! :project) :accessor project)
   (release :initarg :release :initform (arg! :release) :accessor release)
   (systems :initarg :systems :accessor systems)))

(defmethod initialize-instance :after ((release project-release) &key)
  (unless (slot-boundp release 'systems)
    (setf (systems release)
          (loop for asd in (loop for file in (source-files release)
                                 when (string= "asd" (pathname-type file))
                                 collect file)
                append (loop for (name file deps) in (find-file-systems asd)
                             collect (make-instance 'system :project release :name name :file file :dependencies deps))))))

(defmethod name ((release project-release))
  (name (project release)))

(defmethod version ((release project-release))
  (version (release release)))

(defmethod url ((release project-release))
  (format NIL "~a/~a" (url (dist release)) (namestring (path release))))

(defmethod path ((release project-release))
  (make-pathname :name (name release) :type "tgz" :directory `(:relative ,(version path))))

(defmethod prefix ((release project-release))
  (format NIL "~a ~a" (name release) (version release)))

(defmethod source-files ((release project-release))
  (source-files (project release)))

(defclass system ()
  ((project :initarg :project :initarg (arg! :project) :accessor project)
   (name :initarg :name :initarg (arg! :name) :accessor name)
   (file :initarg :file :initarg (arg! :file) :accessor file)
   (dependencies :initarg :dependencies :initarg (arg! :dependencies) :accessor dependencies)))
