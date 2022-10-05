#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.redist)

(defvar *default-output-directory* #p "~/dist/releases/")

(defun write-dist-index (release stream)
  (format stream "name: ~(~a~)
version: ~a
system-index-url: ~a
release-index-url: ~a
archive-base-url: ~a
canonical-distinfo-url: ~a
distinfo-subscription-url: ~a
available-versions-url: ~a"
          (name (dist release))
          (version release)
          (systems-url release)
          (releases-url release)
          (url (dist release))
          (dist-url release)
          (dist-url (dist release))
          (releases-url (dist release))))

(defun write-dist-releases-index (dist stream)
  (dolist (release (releases dist))
    (format stream "~a ~a~%" (version release) (dist-url release))))

(defun write-release-index (release output stream)
  (format stream "# project url size file-md5 content-sha1 prefix [system-file1...system-fileN]~%")
  (dolist (project (projects release))
    (let ((file (merge-pathnames (path project) output)))
      (format stream "~a ~a~a ~a ~a ~a ~a~{ ~a~}~%"
              (name project)
              (url (dist release)) (url project)
              (file-size file)
              (archive-md5 project)
              (source-sha1 project)
              (prefix project)
              (remove-duplicates (loop for system in (systems project) collect (enough-namestring (file system)
                                                                                                  (source-directory (project project))))
                                 :test #'string=)))))

(defun write-system-index (release stream)
  (format stream "# project system-file system-name [dependency1..dependencyN]~%")
  (dolist (project (projects release))
    (dolist (system (systems project))
      (format stream "~a ~a ~(~a~{ ~a~}~)~%"
              (name project) (pathname-name (file system)) (name system)
              (dependencies system)))))

(lquery:define-lquery-function time (node time)
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time time 0)
    (setf (plump:attribute node "datetime") (format NIL "~4,'0d.~2,'0d.~2,'0dT~2,'0d:~2,'0d:~2,'0d" yy mm dd h m s))
    (setf (plump:children node) (plump:make-child-array))
    (plump:make-text-node node (format NIL "~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d" yy mm dd h m s)))
  node)

(defun generate-html (output output-name template &rest args)
  (let ((template (make-pathname :name template :type "ctml" :defaults (merge-pathnames "template/" *here*)))
        (*package* #.*package*))
    (with-open-file (output (make-pathname :name output-name :type "html" :defaults output)
                            :direction :output
                            :if-exists :supersede)
      (plump:serialize (apply #'clip:process (plump:parse template) args) output))))

(defgeneric compile (thing &key))

(defmethod compile ((name symbol) &rest args &key &allow-other-keys)
  (apply #'compile (dist name) args))

(defmethod compile ((dist dist) &rest args &key (version (next-version dist)) update verbose (projects NIL projects-p) (output *default-output-directory*) (if-exists :supersede) &allow-other-keys)
  (remf args :update)
  (remf args :version)
  (remf args :projects)
  (when verbose
    (verbose "Compiling ~a ~a" (name dist) version))
  (let ((release (or (find-release version dist)
                     (if projects-p
                         (make-release dist :update update :version version :verbose verbose :projects projects)
                         (make-release dist :update update :version version :verbose verbose))))
        (success NIL))
    (unwind-protect
         (multiple-value-prog1 (apply #'compile release args)
           (flet ((f (path)
                    (ensure-directories-exist (merge-pathnames path output))))
             (with-open-file (stream (f (dist-path dist))
                                     :direction :output
                                     :if-exists if-exists)
               (write-dist-index release stream))
             (with-open-file (stream (f (releases-path dist))
                                     :direction :output
                                     :if-exists if-exists)
               (write-dist-releases-index dist stream))
             (filesystem-utils:copy-file (merge-pathnames "template/redist.css" *here*) output)
             (generate-html output "index" "index" :dists (list-dists) :projects (list-projects))
             (generate-html (f (path dist)) "index" "dist" :dist dist))
           (setf success T))
      ;; We did not return successfully, so remove the release again.
      (unless success
        ;; FIXME: add delete command to remove files as well.
        ;;        need to be careful to not remove files from shared releases
        (setf (releases dist) (remove release (releases dist)))))))

(defmethod compile ((release release) &key (output *default-output-directory*) (if-exists :supersede) verbose force)
  (when verbose
    (verbose "Compiling release ~a" (version release)))
  (ensure-directories-exist output)
  ;; Assemble files from new releases
  (dolist (project (projects release))
    (compile project :output output :if-exists if-exists :verbose verbose :force force))
  (flet ((f (path)
           (ensure-directories-exist (merge-pathnames path output))))
    (with-open-file (stream (f (dist-path release))
                            :direction :output
                            :if-exists if-exists)
      (write-dist-index release stream))
    (with-open-file (stream (f (releases-path release))
                            :direction :output
                            :if-exists if-exists)
      (write-release-index release output stream))
    (with-open-file (stream (f (systems-path release))
                            :direction :output
                            :if-exists if-exists)
      (write-system-index release stream))
    (generate-html (f (dist-path release)) "index" "release" :release release)
    release))

(defmethod compile ((release project-release) &key (output *default-output-directory*) (if-exists :supersede) verbose force)
  (let ((target (merge-pathnames (path release) output)))
    (when (or force (not (probe-file target)))
      (when verbose
        (verbose "Compiling ~a ~a" (name (project release)) (version release)))
      (handler-bind ((error (lambda (e)
                              (when verbose
                                (verbose "~a" e))
                              (continue e))))
        (update (project release) :version (version release))
        (prog1 (tgz (source-files release) (ensure-directories-exist target)
                    :archive-root (make-pathname :directory (list :relative (prefix release)))
                    :base (source-directory (project release)) :if-exists if-exists)
          (setf (archive-md5 release) (digest (merge-pathnames (path release) output) :md5))
          (generate-html target "index" "project" :project (project release))
          (generate-html target (version release) "project-release" :release release))))))
