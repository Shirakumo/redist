#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.redist)

(defun read-dist-index (stream)
  (let ((table (make-hash-table :test 'equal)))
    (loop for line = (read-line stream NIL NIL)
          while line
          do (cl-ppcre:register-groups-bind (key val) ("^\\s*([^#][^:]*):\\s*(.*?)\\s*$" line)
               (setf (gethash (string-downcase key) table) val)))
    table))

(defun read-dist-releases-index (stream)
  (let ((table (make-hash-table :test 'equal)))
    (loop for line = (read-line stream NIL NIL)
          while line
          do (cl-ppcre:register-groups-bind (key val) ("^\\s*([^#][^ ]*) +(.*?)\\s*$" line)
               (setf (gethash (string-downcase key) table) val)))
    table))

(defun read-release-index (stream)
  (let ((table (make-hash-table :test 'equal)))
    (loop for line = (read-line stream NIL NIL)
          while line
          do (cl-ppcre:register-groups-bind (pure) ("^\\s*([^#].*?)\\s*$" line)
               (destructuring-bind (project url size md5 sha1 prefix &rest systems) (cl-ppcre:split "\\s+" pure)
                 (setf (gethash (string-downcase project) table)
                       (list :name project
                             :url url
                             :file-size size
                             :archive-md5 md5
                             :source-sha1 sha1
                             :prefix prefix
                             :systems systems)))))
    table))

(defun read-system-index (stream)
  (let ((table (make-hash-table :test 'equal)))
    (loop for line = (read-line stream NIL NIL)
          while line
          do (cl-ppcre:register-groups-bind (pure) ("^\\s*([^#].*?)\\s*$" line)
               (destructuring-bind (project system-file system-name &rest dependencies) (cl-ppcre:split "\\s+" pure)
                 (push (list :project project
                             :file system-file
                             :name system-name
                             :dependencies dependencies)
                       (gethash (string-downcase project) table)))))
    table))

(defun fetch (url &optional processor verbose)
  (when verbose (verbose "Fetching ~a" url))
  (let ((data (run-string "curl" "-L" url)))
    (if processor
        (with-input-from-string (stream data)
          (funcall processor stream))
        data)))

(defun replicate-dist (url &key name verbose)
  (let* ((distinfo (fetch url #'read-dist-index verbose))
         (dist-versions (fetch (gethash "available-versions-url" distinfo) #'read-dist-releases-index verbose))
         (name (or name (gethash "name" distinfo)))
         (dist (make-instance 'dist :name name :url (gethash "archive-base-url" distinfo))))
    (loop for url being the hash-values of dist-versions
          do (replicate-dist-version dist url :verbose verbose :disturl url))
    dist))

(defun replicate-dist-version (dist url &key verbose (disturl (format NIL "~a/~a.txt" (url dist) (name dist))))
  (let* ((distinfo (fetch url #'read-dist-index verbose))
         (version (gethash "version" distinfo))
         (releases (fetch (gethash "release-index-url" distinfo) #'read-release-index verbose))
         (systems (fetch (gethash "system-index-url" distinfo) #'read-system-index verbose))
         (project-releases ()))
    (unless (find-release version dist)
      (loop for data being the hash-values of releases
            do (destructuring-bind (&key name url archive-md5 source-sha1 &allow-other-keys) data
                 (let ((project (project name)))
                   (unless project
                     (when verbose (verbose "Creating ~a" name))
                     (let ((source (make-instance 'dist-source 
                                                  :url disturl
                                                  :project name
                                                  :version version)))
                       (setf project (make-instance 'project :name name :sources (list source)))
                       (setf (project name) project)))
                   (let ((release (find-release version project)))
                     (unless release
                       (when verbose (verbose "Creating ~a / ~a" name version))
                       (setf release (make-instance 'project-release
                                                    :project project
                                                    :version version
                                                    :archive-md5 archive-md5
                                                    :source-sha1 source-sha1
                                                    :systems (loop for data in (gethash name systems)
                                                                   collect (list* (getf data :name) data)))))
                     (push release project-releases)))))
      (when verbose (verbose "Creating ~a / ~a" (name dist) version))
      (ensure-release (list version
                            :timestamp (parse-time version :error NIL :default (get-universal-time))
                            :projects project-releases)
                      dist))))
