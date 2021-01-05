#|
 This file is a part of Shirakumo-Dist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defun write-dist-index (release stream)
  (format stream "name: ~a
version: ~a
distinfo-subscription-url: ~a
release-index-url: ~a
system-index-url: ~a"
          (name (dist release)) (version release)
          (url release)
          (releases-url release)
          (systems-url release)))

(defun write-release-index (release output stream)
  (format stream "# project url size file-md5 content-sha1 prefix [system-file1...system-fileN]~%")
  (dolist (project (projects release))
    (let ((file (merge-pathnames (path project) output)))
      (format stream "~a ~a ~a ~a ~a ~a~{ ~a~}~%"
              (name project) (url project)
              (file-size file)
              (digest file :md5)
              (digest (source-files project) :sha1)
              (prefix project)
              (remove-duplicates (mapcar #'name (systems project)) :test #'string=)))))

(defun write-system-index (release stream)
  (format stream "# project system-file system-name [dependency1..dependencyN]~%")
  (dolist (project (projects release))
    (dolist (system (systems project))
      (format stream "~a ~a ~a~{ ~a~}~%"
              (name project) (pathname-name (file system)) (name system)
              (dependencies system)))))

(defgeneric compile (thing &key))

(defmethod compile ((dist dist) &rest args &key &allow-other-keys)
  (apply #'compile (make-release dist) args))

(defmethod compile ((release release) &key output (if-exists :supersede) verbose)
  (ensure-directories-exist output)
  ;; Assemble files
  (dolist (project (projects release))
    (compile project :output output :if-exists if-exists :verbose verbose))
  (flet ((f (&rest format)
           (merge-pathnames (apply #'format NIL format) output)))
    (with-open-file (stream (f "~a.txt" (name release))
                            :direction :output
                            :if-exists if-exists)
      (write-dist-index release stream))
    (with-open-file (stream (f (releases-path release))
                            :direction :output
                            :if-exists if-exists)
      (write-releases-index release output stream))
    (with-open-file (stream (f (systems-path release))
                            :direction :output
                            :if-exists if-exists)
      (write-system-index release stream))))

(defmethod compile ((release project-release) &key output (if-exists :supersede) verbose)
  (when verbose
    (format T "~& Compiling ~a~%" release))
  (tgz (source-files release) (ensure-directories-exist (merge-pathnames (path release) output))
       :if-exists if-exists))
