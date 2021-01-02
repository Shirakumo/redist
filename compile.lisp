#|
 This file is a part of Shirakumo-Dist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defun create-dist-index (release)
  (format NIL "name: ~a
version: ~a
distinfo-subscription-url: ~a
release-index-url: ~a
system-index-url: ~a"
          (dist release) (version release)
          (url release)
          (release-url release)
          (system-url release)))

(defun create-release-index (release)
  (with-output-to-string (stream)
    (format stream "# project url size file-md5 content-sha1 prefix [system-file1...system-fileN]~%")
    (dolist (project (projects release))
      (format stream "~a ~a ~a ~a ~a ~a~{ ~a~}~%"
              (name project) (url project)
              (file-size (file project))
              (md5 (file project))
              (sha1 (file project))
              (prefix project)
              (remove-duplicates (mapcar #'name (systems project)) :test #'string=)))))

(defun create-system-index (release)
  (with-output-to-string (stream)
    (format stream "# project system-file system-name [dependency1..dependencyN]~%")
    (dolist (project (projects release))
      (dolist (system (systems project))
        (format stream "~a ~a ~a~{ ~a~}~%"
                (name project) (pathname-name (file system)) (name system)
                (dependencies system))))))
