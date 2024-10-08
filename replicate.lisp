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

(defun replicate-dist (disturl &key name (type 'timestamp-versioned-dist) (verbose T) (download-archives T) current-version-only)
  (let* ((distinfo (fetch disturl #'read-dist-index verbose))
         (name (or name (gethash "name" distinfo)))
         (dist (ensure-instance (dist name) type :name name :url (gethash "archive-base-url" distinfo))))
    (if current-version-only
        (replicate-dist-version dist disturl :verbose verbose :disturl disturl :download-archives download-archives)
        (let* ((versions (fetch (available-versions-url distinfo) #'read-dist-releases-index verbose))
               (versions (loop for url being the hash-values of versions collect url)))
          (do-list* (url versions)
            (replicate-dist-version dist url :verbose verbose :disturl disturl :download-archives download-archives))))
    (setf (dist name) dist)))

(defun replicate-dist-version (dist url &key (verbose T) disturl (download-archives T))
  (let* ((distinfo (fetch url #'read-dist-index verbose))
         (disturl (or disturl (format NIL "~a/~a.txt" (url dist) (name dist))))
         (version (gethash "version" distinfo))
         (releases (fetch (gethash "release-index-url" distinfo) #'read-release-index verbose))
         (releases (loop for data being the hash-values of releases collect data))
         (systems (fetch (gethash "system-index-url" distinfo) #'read-system-index verbose))
         (dist-release (find-release version dist))
         (project-releases ()))
    (unless dist-release
      (do-list* (data releases)
        (destructuring-bind (&key name url archive-md5 source-sha1 &allow-other-keys) data
          (let ((project (project name))
                (version source-sha1)
                (source (make-instance 'dist-source :url disturl :project name)))
            (cond ((null project)
                   (when verbose (verbose "Creating ~a" name))
                   (setf project (make-instance 'project :name name :sources (list source) :verbose verbose))
                   (setf (project name) project))
                  ((loop for source in (sources project) never (typep source 'dist-source))
                   (when verbose (verbose "Adding dist source to ~a" name))
                   (reinitialize-instance project :sources (append (sources project) (list source))
                                                  :disabled-p NIL)))
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
                (push release (releases project))
                (when download-archives
                  (let ((target (merge-pathnames (path release) (default-output-directory))))
                    (unless (probe-file target)
                      (ensure-directories-exist target)
                      (run "curl" "-L" "-o" target url)))))
              (push release project-releases)))))
      (when verbose (verbose "Creating ~a / ~a" (name dist) version))
      (setf dist-release (ensure-release (list version
                                               :timestamp (parse-time version :error NIL :time-zone 0 :default (get-universal-time))
                                               :projects project-releases)
                                         dist))
      (push dist-release (releases dist)))
    dist-release))
