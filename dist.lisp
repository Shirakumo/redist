#|
 This file is a part of Shirakumo-Dist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defgeneric make-release (thing &key))

(defclass dist ()
  (name
   projects))

(defmethod make-release ((dist dist) &key (version (next-version dist)))
  (make-instance 'release
                 :dist dist
                 :version version))

(defclass release ()
  (dist
   version
   projects))

(defmethod initialize-instance :after ((release release) &key dist version projects)
  (unless projects
    (setf (slot-value release 'projects)
          (loop for project in (projects dist)
                collect (make-release project :release release)))))

(defclass project ()
  (name
   remote))

(defmethod make-release ((project project) &key release)
  (make-instance 'project-release
                 :project project
                 :release release
                 :systems (find-systems project)))

(defmethod find-systems ((project project))
  (loop for (name file deps) in (find-all-systems (source-directory project))
        collect (make-instance 'system :project project :name name :file file :dependencies deps)))

(defclass project-release ()
  (project
   release
   systems))

(defclass system ()
  (project
   name
   file
   dependencies))
