#|
 This file is a part of Shirakumo-Dist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defclass dist ()
  (name
   projects))

(defclass release ()
  (dist
   version
   projects))

(defclass project ()
  (name
   remote))

(defclass project-release ()
  (project
   release
   systems))

(defclass system ()
  (project
   name
   file
   dependencies))
