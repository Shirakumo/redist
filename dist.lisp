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
   (projects :initarg :projects :initform (arg! :dist) :accessor projects)))

(defmethod make-release ((dist dist) &key (version (next-version dist)))
  (make-instance 'release
                 :dist dist
                 :version version))

(defclass release ()
  ((dist :initarg :dist :initform (arg! :dist) :accessor dist)
   (version :initarg :version :initform (next-version) :accessor version)
   (projects :initarg :projects :accessor projects)))

(defmethod initialize-instance :after ((release release) &key dist version)
  (unless (slot-boundp release 'projects)
    (setf (projects release)
          (loop for project in (projects dist)
                collect (make-release project :release release)))))

(defclass project ()
  ((name :initarg :name :initform (arg! :name) :accessor name)
   (source-directory :initarg :source-directory :initform (arg! :source-directory) :accessor source-directory)))

(defmethod make-release ((project project) &key release)
  (make-instance 'project-release
                 :project project
                 :release release))

(defclass project-release ()
  ((project :initarg :project :initform (arg! :project) :accessor project)
   (release :initarg :release :initform (arg! :release) :accessor release)
   (systems :initarg :systems :accessor systems)
   (source-directory :initarg :source-directory :accessor source-directory)))

(defmethod initialize-instance :after ((release project-release) &key project)
  (unless (slot-boundp release 'systems)
    (setf (systems release)
          (loop for (name file deps) in (find-all-systems (source-directory release))
                collect (make-instance 'system :project release :name name :file file :dependencies deps)))))

(defclass system ()
  ((project :initarg :project :initarg (arg! :project) :accessor project)
   (name :initarg :name :initarg (arg! :name) :accessor name)
   (file :initarg :file :initarg (arg! :file) :accessor file)
   (dependencies :initarg :dependencies :initarg (arg! :dependencies) :accessor dependencies)))
