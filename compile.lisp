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

(defun tar (files output &key (if-exists :error))
  (archive:with-open-archive (archive output
                              :direction :output
                              :if-exists if-exists)
    (dolist (file files)
      (let ((entry (archive:create-entry-from-pathname archive file)))
        (archive:write-entry-to-archive archive entry)))
    (archive:finalize-archive archive)
    output))

(defun gz (file output &key (if-exists :error))
  (salza2:gzip-file file output :if-exists if-exists))

(defun tgz (files output &key (if-exists :error))
  (let ((tar (make-pathname :type "tar" :defaults output)))
    (tar files tar :if-exists :error)
    (unwind-protect
         (gz tar output :if-exists if-exists)
      (delete-file tar))))
