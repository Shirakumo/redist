#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.redist)

(defun create-update-script (&key (file (merge-pathnames "redist.lisp" *distinfo-file*)) (if-exists :supersede))
  (ensure-directories-exist file)
  (with-open-file (stream file :direction :output :if-exists if-exists)
    (with-standard-io-syntax
      (let ((*package* #.*package*)
            (*print-case* :downcase)
            (*print-right-margin* 80)
            (*print-readably* NIL))
        (format stream "~&#| Dist update script compiled automatically~%")
        (format stream "~&~a --noinform --load \"$0\" -- \"$@\"; exit~%" (first (uiop:raw-command-line-arguments)))
        (format stream "~&# |#~%~%")
        (format stream "~&#-quicklisp (load ~s)~%" (ql-impl-util::quicklisp-init-file-form))
        (format stream "~&(ql:quickload :redist :silent T)~%~%")
        (format stream "~&(setf org.shirakumo.redist:*distinfo-file* ~s)~%" *distinfo-file*)
        (format stream "~&(setf org.shirakumo.redist:*default-output-directory* ~s)~%" *default-output-directory*)
        (format stream "~&(setf org.shirakumo.redist:*default-source-directory* ~s)~%" *default-source-directory*)
        (format stream "~&(org.shirakumo.redist:main (rest (uiop:command-line-arguments)))~%"))))
  file)

(defun main/help ()
  (format T "Commands:

compile               Compile a dist release
  -v --version version   Specify the version to compile
  -d --dist dist         Specify the dist to compile. Can be specified
                         multiple times. If unspecified, all dists are
                         compiled
  -u --update            If specified will update associated projects
                         first
  -v --verbose           If specified shows updates about the progress
  -f --force             If specified compiles the dist version even
                         if it exists already

update                Update local project checkouts
  -v --version version   Specify the version to update to. If
                         unspecified, updates to latest
  -p --project project   Specify the project to update. Can be
                         specified multiple times. If unspecified, all
                         projects are updated
  -v --verbose           If specified shows updates about the progress

list                  List known objects
  thing                  The thing to list. Can be projects, dists, or
                         releases
  -p --project project   The project to list releases of
  -d --dist dist         The dist to list releases of

add                   Add a new project or add a project to a dist
  url                    The url of the project's primary remote
  -t --type type         The type of the remote source. Defaults to
                         GIT
  -n --name name         The name of the project. If unspecified is
                         derived from the url
  -d --dist dist         The dist to add the project to. Can be
                         specified multiple times. If unspecified,
                         adds to all dists

remove                Remove a project from dists
  name                   The name of the project to remove
  -d --dist dist         The dist to remove the project from. Can be
                         specified multiple times. If unspecified,
                         removes from all dists

help                  Shows this help listing
"))

(defun main/compile (&key version update dist verbose force)
  (let ((args (list* :if-exists :supersede :force force :update update :verbose verbose
                     (if version (list :version version)))))
    (dolist (dist (or (enlist dist) (list-dists)))
      (apply #'compile dist args))))

(defun main/update (&key version project verbose)
  (dolist (project (or (enlist project) (list-projects)))
    (update project :version version :verbose verbose)))

(defun main/list (thing &key project dist)
  (cond ((string= thing "projects")
         (dolist (project (list-projects))
           (format T "~&~a~%" project)))
        ((string= thing "dists")
         (dolist (dist (list-dists))
           (format T "~&~a~%" dist)))
        ((string= thing "releases")
         (dolist (release (releases (cond (project (project project))
                                          (dist (dist dist))
                                          (T (error "Must pass either DIST or PROJECT.")))))
           (format T "~&~a~%" (version release))))
        ((string= thing "sources")
         (dolist (dist (list-dists))
           (format T "~&~a~%" dist)))
        (T
         (error "Don't know how to list ~a." thing))))

(defun main/add (url &key type name dist)
  (let* ((name (or name (url-extract-name url)))
         (type (intern (string-upcase (or type "git")) "KEYWORD"))
         (project (or (project name)
                      (make-instance 'project :name name :sources `((,type ,url))))))
    (dolist (dist (or (enlist dist) (list-dists)))
      (add-project project dist))))

(defun main/remove (name &key dist)
  (let ((project (or (project name)
                     (error "No project named ~s" name))))
    (dolist (dist (or (enlist dist) (list-dists)))
      (remove-project project))))

(defun parse-args (args &key flags chars)
  (let ((kargs ())
        (pargs ()))
    (loop for arg = (pop args)
          while arg
          do (flet ((handle-argument (arg)
                      (setf (getf kargs arg) (cond ((find arg flags :test #'string-equal)
                                                    T)
                                                   ((null (getf kargs arg))
                                                    (pop args))
                                                   ((consp (getf kargs arg))
                                                    (list* (pop args) (getf kargs arg)))
                                                   (T
                                                    (list (pop args) (getf kargs arg)))))))
               (cond ((string= "--" arg :end2 2)
                      (handle-argument (find-symbol (string-upcase (subseq arg 2)) "KEYWORD")))
                     ((string= "-" arg :end2 1)
                      (loop for char across (subseq arg 1)
                            for arg = (getf chars char)
                            do (cond (arg
                                      (handle-argument arg))
                                     (T
                                      (error "No such argument ~a" char)))))
                     (T
                      (push arg pargs)))))
    (append (nreverse pargs) kargs)))

(defun main (&optional (args (uiop:command-line-arguments)))
  (let ((args (or args '("help"))))
    (restore)
    (destructuring-bind (command . args) args
      (let ((cmdfun (find-symbol (format NIL "~a/~:@(~a~)" 'main command) #.*package*)))
        (if cmdfun
            (apply #'funcall cmdfun (parse-args args :flags '(:verbose :update :force)
                                                     :chars '(#\v :verbose #\u :update #\f :force
                                                              #\d :dist #\p :project #\n :version
                                                              #\n :name #\t :type)))
            (error "No command named ~s." command))))
    (persist)
    (uiop:quit)))