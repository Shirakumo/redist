(in-package #:org.shirakumo.redist)

(defgeneric parse-quicklisp-source (type args))

(defmacro do-quicklisp-file ((line file &optional result) &body body)
  `(with-open-file (stream ,file)
     (loop for ,line = (read-line stream NIL NIL)
           while ,line
           do (let ((,line (string-trim '(#\Space #\Linefeed #\Return) (subseq ,line 0 (or (position #\# ,line) (length ,line))))))
                (when (string/= "" ,line)
                  ,@body)))
     ,result))

(defun parse-quicklisp-projects (root &key (source-directory (default-source-directory)) (if-does-not-exist :create) (on-error :disable))
  (let ((excluded-systems (make-hash-table :test #'equalp))
        (excluded-paths ()))
    (do-quicklisp-file (line (merge-pathnames "qlc-meta/excluded-systems.txt" root))
      (destructuring-bind (project &rest systems) (cl-ppcre:split " +" line)
        (dolist (system systems) (push system (gethash project excluded-systems)))))
    (do-quicklisp-file (line (merge-pathnames "qlc-meta/excluded-system-pathnames.txt" root))
      (if (char= #\/ (char line 0))
          (push (pathname-utils:parse-native-namestring (subseq line 1)) excluded-paths)
          (push (pathname-utils:parse-native-namestring line) excluded-paths)))
    (do-list* (dir (directory (merge-pathnames "projects/*/" root)))
      (let* ((name (pathname-utils:directory-name dir))
             (sources (parse-quicklisp-source-file (merge-pathnames "source.txt" dir)))
             (project (project name)))
        (unless project
          (ecase if-does-not-exist
            (:create)
            (:error (error "No project named ~s" name))
            (:ignore (setf project :ignore))))
        (unless (eql :ignore project)
          (handler-bind ((error (lambda (e)
                                  (declare (ignore e))
                                  (ecase on-error
                                    (:disable
                                     (verbose "Disabling ~a" name)
                                     (invoke-restart 'disable))
                                    (:remove
                                     (verbose "Removing ~a" name)
                                     (invoke-restart 'remove))
                                    (:error)))))
            (restart-case
                (let ((project (ensure-instance project 'project
                                                :name name
                                                :sources sources
                                                :source-directory (merge-pathnames (make-pathname :directory (list :relative name)) source-directory)
                                                :excluded-systems (gethash name excluded-systems))))
                  (setf (project name) project))
              (remove ()
                :report "Remove the project"
                (setf (project name) NIL)))))))))

(defun parse-quicklisp-source-file (file)
  (let ((managers ()))
    (do-quicklisp-file (line file managers)
      (destructuring-bind (type &rest args) (cl-ppcre:split " +" line)
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
