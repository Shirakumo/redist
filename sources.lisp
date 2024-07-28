(in-package #:org.shirakumo.redist)

(defclass source-manager ()
  ((url :initarg :url :initform (arg! :url) :accessor url)))

(defmethod print-object ((manager source-manager) stream)
  (print-unreadable-object (manager stream :type T)
    (format stream "~a" (url manager))))

(defgeneric version (manager))
(defgeneric update (manager &key))
(defgeneric clone (manager &key))

(defmethod clone :before ((manager source-manager) &key verbose &allow-other-keys)
  (when verbose
    (verbose "Cloning from ~a ~a" (type-of manager) (url manager))))

(defmethod update :before ((manager source-manager) &key verbose version &allow-other-keys)
  (when verbose
    (if version
        (verbose "Checking out to ~a" version)
        (verbose "Updating from ~a" (url manager)))))

(defmethod update :around ((manager source-manager) &key &allow-other-keys)
  (with-simple-restart (continue "Ignore the update and continue as if it had happened.")
    (call-next-method)))

(defmethod version ((manager source-manager))
  (digest (gather-sources simple-inferiors:*cwd*) :sha1))

(defmethod ensure-source ((source source-manager))
  source)

(defmethod ensure-source ((spec cons))
  (destructuring-bind (type url . initargs) spec
    (etypecase type
      ((or keyword string)
       (setf type (or (find-symbol (string type) #.*package*)
                      (error "No source type with name ~s found!" type))))
      (symbol))
    (apply #'make-instance type :url url initargs)))

(defmethod serialize append ((source source-manager))
  (list (type-of source) (url source)))

(defclass cvs (source-manager)
  ())

(defmethod clone ((manager cvs) &key version shallow)
  (declare (ignore shallow))
  (run "cvs" "-d" (url manager) "checkout"
       "-D" (or version "1 second ago")
       "."))

(defmethod update ((manager cvs) &key version)
  (run "cvs" "-d" (url manager) "update"
       "-d" "-D" (or version "1 second ago")))

(defclass svn (source-manager)
  ())

(defmethod clone ((manager svn) &key version shallow)
  (declare (ignore shallow))
  (run "svn" "checkout"
       "-r" (or version "HEAD")
       (url manager)
       "."))

(defmethod update ((manager svn) &key version)
  (run "svn" "update"
       "-r" (or version "HEAD")
       "."))

(defmethod version ((manager svn))
  (run-string "svn" "info"
              "--show-item" "last-changed-revision"))

(defclass darcs (source-manager)
  ())

(defmethod clone ((manager darcs) &key version shallow)
  (let ((name (pathname-utils:directory-name simple-inferiors:*cwd*)))
    (uiop:delete-empty-directory simple-inferiors:*cwd*)
    (simple-inferiors:with-chdir ((pathname-utils:parent simple-inferiors:*cwd*))
      (run "darcs" "clone"
           (when shallow (list "--lazy"))
           (when version (list "--tag" version))
           (url manager) name))))

(defmethod update ((manager darcs) &key version)
  (when version (error "Not supported yet."))
  (run "darcs" "pull"))

(defmethod version ((manager darcs))
  (with-input-from-string (stream (run-string "darcs" "log" "--last" "1"))
    (let ((line (read-line stream)))
      (subseq line (1+ (position #\Space line))))))

(defclass mercurial (source-manager)
  ())

(defmethod clone ((manager mercurial) &key version shallow)
  (declare (ignore shallow))
  (run "hg" "clone"
       (when version "-r" version)
       (url manager) "."))

(defmethod update ((manager mercurial) &key version)
  (run "hg" "pull")
  (when version
    (run "hg" "update" "-r" version)))

(defmethod version ((manager mercurial))
  (run-string "hg" "id" "-i"))

(defclass git (source-manager)
  ((branch :initarg :branch :initform NIL :accessor branch)
   (tag :initarg :tag :initform NIL :accessor tag)))

(defmethod serialize append ((manager git))
  (prune-plist
   (list :branch (branch manager)
         :tag (tag manager))))

(defmethod clone ((manager git) &key version shallow)
  (run "git" "clone"
       (when (branch manager) (list "--branch" (branch manager)))
       (when shallow (list "--depth" "1"))
       (url manager) ".")
  (when (or version (tag manager))
    (update manager :version version)))

(defmethod update ((manager git) &key version)
  (cond (version
         (run "git" "checkout" version))
        (T
         (run "git" "fetch" "origin")
         (run "git" "reset" "--hard" (cond ((tag manager)
                                            (if (eql :latest (tag manager))
                                                (simple-inferiors:run "git" (list "describe" "--tags" "--abbrev=0") :output :string)
                                                (tag manager)))
                                           ((branch manager) (format NIL "origin/~a" (branch manager)))
                                           (T (run-string "git" "rev-parse" "--abbrev-ref" "origin/HEAD")))))))

(defmethod version ((manager git))
  (run-string "git" "rev-parse" "HEAD"))

(defun download-source (url &key strip-root)
  (let* ((name (subseq url (1+ (position #\/ url :from-end T))))
         (temp (tempfile)))
    ;; Might as well use these, since we already need all the other binary utilities...
    (unwind-protect
         (progn
           (run "curl" "-L" "-o" temp url)
           (cond ((or (ends-with ".tar.gz" name)
                      (ends-with ".tgz" name)
                      (ends-with ".zip" name))
                  (run "bsdtar" "-xf" temp "-C" "." (when strip-root "--strip-components=1")))
                 ((or (ends-with ".lisp" name)
                      (ends-with ".lsp" name))
                  (let* ((local (merge-pathnames name simple-inferiors:*cwd*))
                         (name (pathname-name local)))
                    (uiop:copy-file temp local)
                    (with-open-file (stream (make-pathname :type "asd" :defaults local)
                                            :direction :output
                                            :if-exists NIL)
                      (when stream
                        (format stream "~
;;;; Automatically generated for a single-file source.
(asdf:defsystem #:~a
  :homepage ~s
  :source-control (:http ~:*~s)
  :components
  ((:file ~s)))~%" name url name)))))
                 (T
                  (error "Don't know how to deal with file ~a" name))))
      (when (probe-file temp)
        (delete-file temp)))))

(defclass http (source-manager)
  ())

(defmethod clone ((manager http) &key version shallow)
  (declare (ignore version shallow))
  (download-source (url manager)))

(defmethod update ((manager http) &rest args &key &allow-other-keys)
  (apply #'clone manager args))

(defclass github (git)
  ((track :initarg :track :initform (arg! :track) :accessor track)))

(defmethod serialize append ((manager github))
  (list :track (track manager)))

(defmethod path ((manager github))
  (subseq (url manager)
          (+ (length "github.com/") (search "github.com/" (url manager)))
          (or (search ".git" (url manager) :from-end T) (length (url manager)))))

(defmethod tag ((manager github))
  (ecase (track manager)
    (:release
     (let ((content (shasht:read-json (run-string "curl" (format NIL "https://api.github.com/repos/~a/releases/latest" (path manager))))))
       (gethash "tag_name" content)))))

(defclass gitlab (git)
  ((track :initarg :track :initform (arg! :track) :accessor track)
   (token :initarg :token :initform NIL :accessor token)))

(defmethod serialize append ((manager gitlab))
  (prune-plist
   (list :track (track manager)
         :token (token manager))))

(defmethod path ((manager gitlab))
  (subseq (url manager)
          (1+ (position #\/ (url manager) :start (length "https://")))
          (or (search ".git" (url manager) :from-end T) (length (url manager)))))

(defmethod instance ((manager gitlab))
  (subseq (url manager)
          0
          (position #\/ (url manager) :start (length "https://"))))

(defmethod tag ((manager gitlab))
  (ecase (track manager)
    (:release
     (let* ((url (format NIL "~a/api/v4/projects/~a/releases" (instance manager) (url-encode (path manager))))
            (content (shasht:read-json (if (token manager)
                                           (run-string "curl" "--header" (format NIL "PRIVATE-TOKEN: ~a" (token manager)) url)
                                           (run-string "curl" url)))))
       (gethash "tag_name" (aref content 0))))))

(defclass dist-source (source-manager)
  ((project :initarg :project :initform (arg! :project) :accessor project)
   (version :initarg :version :initform NIL :accessor version)))

(defmethod clone ((manager dist-source) &key version shallow)
  (declare (ignore shallow))
  (let* ((index (fetch (url manager) #'read-dist-index))
         (versions (gethash "available-versions-url" index)))
    (when (and version versions)
      (let* ((versions (fetch versions #'read-dist-releases-index)))
        (unless (gethash version versions)
          (error "The dist doesn't appear to have a version ~s" version))
        (setf index (fetch (url manager) #'read-dist-index))))
    (when (or (null (version manager)) (not (equal (version manager) (gethash "version" index))))
      (let ((releases (gethash "release-index-url" index)))
        (unless releases
          (error "Doesn't appear to be a valid dist url."))
        (let* ((releases (fetch releases #'read-release-index))
               (project (gethash (project manager) releases)))
          (unless project
            (error "The dist doesn't appear to have a project named ~s" (project manager)))
          (prog1 (download-source (getf project :url) :strip-root T)
            (setf (version manager) (gethash "version" index))))))))

(defmethod update ((manager dist-source) &rest args &key &allow-other-keys)
  (apply #'clone manager args))

(defmethod serialize append ((manager dist-source))
  (prune-plist
   (list :project (project manager)
         :version (version manager))))
