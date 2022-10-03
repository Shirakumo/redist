#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.redist)

(defclass cvs (source-manager)
  ())

(defmethod clone ((manager cvs) &key version)
  (run "cvs" "-d" (url manager) "checkout" "-D" (or version "1 second ago") "."))

(defmethod update ((manager cvs) &key version)
  (run "cvs" "-d" (url manager) "update" "-d" "-D" (or version "1 second ago")))

(defclass svn (source-manager)
  ())

(defmethod clone ((manager svn) &key version)
  (run "svn" "checkout" "-r" (or version "HEAD") (url manager) "."))

(defmethod update ((manager svn) &key version)
  (run "svn" "update" "-r" (or version "HEAD") "."))

(defmethod version ((manager svn))
  (run-string "svn" "info" "--show-item" "last-changed-revision"))

(defclass darcs (source-manager)
  ())

(defmethod clone ((manager darcs) &key version)
  (let ((name (pathname-utils:directory-name simple-inferiors:*cwd*)))
    (uiop:delete-empty-directory simple-inferiors:*cwd*)
    (simple-inferiors:with-chdir ((pathname-utils:parent simple-inferiors:*cwd*))
      (if version
          (run "darcs" "clone" "--tag" version (url manager) name)
          (run "darcs" "clone" (url manager) name)))))

(defmethod update ((manager darcs) &key version)
  (when version (error "Not supported yet."))
  (run "darcs" "pull"))

(defmethod version ((manager darcs))
  (with-input-from-string (stream (run-string "darcs" "log" "--last" "1"))
    (let ((line (read-line stream)))
      (subseq line (1+ (position #\Space line))))))

(defclass mercurial (source-manager)
  ())

(defmethod clone ((manager mercurial) &key version)
  (if version
      (run "hg" "clone" "-r" version (url manager) ".")
      (run "hg" "clone" (url manager) ".")))

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

(defmethod clone ((manager git) &key version)
  (if (branch manager)
      (run "git" "clone" "--branch" (branch manager) (url manager) ".")
      (run "git" "clone" (url manager) "."))
  (when (or version (tag manager))
    (update manager :version version)))

(defmethod update ((manager git) &key version)
  (run "git" "fetch" "origin")
  (run "git" "reset" "--hard" (cond (version version)
                                    ((tag manager)
                                     (if (eql :latest (tag manager))
                                         (simple-inferiors:run "git" (list "describe" "--tags" "--abbrev=0") :output :string)
                                         (tag manager)))
                                    ((branch manager) (format NIL "origin/~a" (branch manager)))
                                    (T (format NIL "origin/~a" (run-string "git" "branch" "--show-current"))))))

(defmethod version ((manager git))
  (run-string "git" "rev-parse" "HEAD"))

(defclass http (source-manager)
  ())

(defmethod clone ((manager http) &key version)
  (declare (ignore version))
  (let* ((name (subseq (url manager) (1+ (position #\/ (url manager) :from-end T))))
         (temp (tempfile)))
    ;; Might as well use these, since we already need all the other binary utilities...
    (unwind-protect
         (progn
           (run "curl" "-L" "-o" (uiop:native-namestring temp) (url manager))
           (cond ((or (ends-with ".tar.gz" name)
                      (ends-with ".tgz" name))
                  (run "tar" "-xf" (uiop:native-namestring temp) "-C" "."))
                 ((or (ends-with ".zip" name))
                  (run "unzip" (uiop:native-namestring temp) "-d" "."))
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
  ((:file ~s)))~%" name (url manager) name)))))
                 (T
                  (error "Don't know how to deal with file ~a" name))))
      (when (probe-file temp)
        (delete-file temp)))))

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
