#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.redist)

(defgeneric parse-quicklisp-source (type args))

(defmacro do-quicklisp-file ((line file &optional result) &body body)
  `(with-open-file (stream ,file)
     (loop for ,line = (read-line stream NIL NIL)
           while ,line
           do (let ((,line (string-trim " " (subseq ,line 0 (or (position #\# ,line) (length ,line))))))
                (when (string/= "" ,line)
                  ,@body)))
     ,result))

(defun parse-quicklisp-projects (root &key (source-directory (default-source-directory)))
  (let ((excluded-systems (make-hash-table :test #'equalp))
        (excluded-paths ()))
    (do-quicklisp-file (line (merge-pathnames "qlc-meta/excluded-systems.txt" root))
      (destructuring-bind (project &rest systems) (split #\  line)
        (dolist (system systems) (push system (gethash project excluded-systems)))))
    (do-quicklisp-file (line (merge-pathnames "qlc-meta/excluded-system-pathnames.txt" root))
      (if (char= #\/ (char line 0))
          (push (uiop:parse-native-namestring (subseq line 1)) excluded-paths)
          (push (uiop:parse-native-namestring line) excluded-paths)))
    (loop for dir in (directory (merge-pathnames "projects/*/" root))
          for name = (pathname-utils:directory-name dir)
          for sources = (parse-quicklisp-source-file (merge-pathnames "source.txt" dir))
          do (setf (project name) (ensure-instance (project name) 'project
                                                   :name name
                                                   :sources sources
                                                   :source-directory (merge-pathnames (make-pathname :directory (list :relative name)) source-directory)
                                                   :excluded-systems (gethash name excluded-systems))))))

(defun parse-quicklisp-source-file (file)
  (let ((managers ()))
    (do-quicklisp-file (line file managers)
      (destructuring-bind (type &rest args) (split #\  line)
        (let ((manager (parse-quicklisp-source (intern (string-upcase type) #.*package*) args)))
          (when manager (push manager managers)))))))

(defmacro define-quicklisp-source-parser (type args &body body)
  `(defmethod parse-quicklisp-source ((type (eql ',type)) args)
     (flet ((parse-quicklisp-source (type &rest args)
              (parse-quicklisp-source type args)))
       (declare (ignorable #'parse-quicklisp-source))
       (destructuring-bind ,args args
         ,@body))))

(define-quicklisp-source-parser \. ())

(define-quicklisp-source-parser cvs (source)
  (make-instance 'cvs :url source))

(define-quicklisp-source-parser svn (source)
  (make-instance 'svn :url source))

(define-quicklisp-source-parser darcs (source)
  (make-instance 'darcs :url source))

(define-quicklisp-source-parser mercurial (source)
  (make-instance 'mercurial :url source))

(define-quicklisp-source-parser git (source &optional branch tag)
  (make-instance 'git :url source :branch branch :tag tag))

(define-quicklisp-source-parser http (url)
  (make-instance 'http :url url))

(define-quicklisp-source-parser https (url)
  (make-instance 'http :url url))

(define-quicklisp-source-parser single-file (url)
  (make-instance 'http :url url))

(define-quicklisp-source-parser tagged-git (source tag)
  (make-instance 'git :url source :tag tag))

(define-quicklisp-source-parser branched-git (source branch)
  (make-instance 'git :url source :branch branch))

(define-quicklisp-source-parser latest-github-release (source)
  (make-instance 'github :url source :track :release))

(define-quicklisp-source-parser latest-github-tag (source)
  (make-instance 'git :url source :tag :latest))

(define-quicklisp-source-parser latest-gitlab-release (source)
  (make-instance 'gitlab :url source :track :release))

;;; Wtf?
(define-quicklisp-source-parser ediware-http (name)
  (make-instance 'git :url (format NIL "https://github.com/edicl/~a.git" name) :tag :latest))

(define-quicklisp-source-parser kmr-git (name)
  (make-instance 'git :url (format NIL "http://git.kpe.io/~a.git/" name)))
