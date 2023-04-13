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

(defun available-versions-url (distinfo)
  (or (gethash "available-versions-url" distinfo)
      (cl-ppcre:register-groups-bind (base ext) ("^(.*?)(\\.[^.]+)?$" (gethash "distinfo-subscription-url" distinfo))
        (format NIL "~a-versions~@[~a~]" base ext))))

(defun replicate-dist (disturl &key name (verbose T) (download-archives T) current-version-only)
  (let* ((distinfo (fetch disturl #'read-dist-index verbose))
         (name (or name (gethash "name" distinfo)))
         (dist (ensure-instance (dist name) 'dist :name name :url (gethash "archive-base-url" distinfo))))
    (if current-version-only
        (replicate-dist-version dist disturl :verbose verbose :disturl disturl :download-archives download-archives)
        (loop for url being the hash-values of (fetch (available-versions-url "available-versions-url") #'read-dist-releases-index verbose)
              do (replicate-dist-version dist url :verbose verbose :disturl disturl :download-archives download-archives)))
    (setf (dist name) dist)))

(defun replicate-dist-version (dist url &key (verbose T) disturl (download-archives T))
  (let* ((distinfo (fetch url #'read-dist-index verbose))
         (disturl (or disturl (format NIL "~a/~a.txt" (url dist) (name dist))))
         (version (gethash "version" distinfo))
         (releases (fetch (gethash "release-index-url" distinfo) #'read-release-index verbose))
         (systems (fetch (gethash "system-index-url" distinfo) #'read-system-index verbose))
         (dist-release (find-release version dist))
         (project-releases ()))
    (unless dist-release
      (loop for data being the hash-values of releases
            do (destructuring-bind (&key name url archive-md5 source-sha1 &allow-other-keys) data
                 (let ((project (project name)))
                   (unless project
                     (when verbose (verbose "Creating ~a" name))
                     (let ((source (make-instance 'dist-source :url disturl :project name)))
                       (setf project (make-instance 'project :name name :sources (list source) :verbose verbose))
                       (setf (project name) project)))
                   (pushnew project (projects dist))
                   (let ((release (find-release version project)))
                     (unless release
                       (when verbose (verbose "Creating ~a / ~a" name version))
                       (setf release (make-instance 'project-release
                                                    :project project
                                                    :version version
                                                    :archive-md5 archive-md5
                                                    :source-sha1 source-sha1
                                                    :systems (loop for data in (gethash name systems)
                                                                   collect (list* (getf data :name) data))))
                       (when download-archives
                         (let ((target (merge-pathnames (path release) (default-output-directory))))
                           (ensure-directories-exist target)
                           (run "curl" "-L" "-o" target url))))
                     (push release project-releases)))))
      (when verbose (verbose "Creating ~a / ~a" (name dist) version))
      (setf dist-release (ensure-release (list version
                                               :timestamp (parse-time version :error NIL :default (get-universal-time))
                                               :projects project-releases)
                                         dist))
      (push dist-release (releases dist)))
    dist-release))
