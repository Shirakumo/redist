#|
 This file is a part of Shirakumo-Dist
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defvar *server-port* 8081
  "Port on which the dist server will run.")
(defvar *dist-name* "shirakumo"
  "The name of the dist.")
(defvar *dist-url* "http://dist.tymoon.eu/"
  "The outside-world-available URL to your dist.")
(defvar *releases-dir*
  (asdf:system-relative-pathname :shirakumo-dist "release" :type :directory)
  "The directory pathname pointing to where the release information will be stored.")
(defvar *dispatcher*
  (hunchentoot:create-folder-dispatcher-and-handler "/" *releases-dir*)
  "The folder dispatcher for the releases.")
(defvar *acceptor* NIL
  "The hunchentoot server acceptor to keep track of the running instance.")

;;;; HACK QUICKDIST FOR .GIT FOLDERS
;; We have to do this to exclude the
;; .git history folder that we don't want
;; to distribute alongside each archive
;; as it would needlessly waste space
;;
;; We also change the format of the
;; archive timestamp to match that of our
;; versioning scheme.
(defun quickdist::archive (destdir-path source-path)
  (let* ((name (format nil "~a-~a" (quickdist::last-directory source-path) (timestamp)))
         (out-path (make-pathname :name name :type "tgz" :defaults (truename destdir-path))))
    (external-program:run quickdist:*gnutar*
                          (list "-C" (quickdist::native-namestring source-path) "."
                                "--exclude" "./.git"
                                "-czf" (quickdist::native-namestring out-path)
                                "--transform" (format nil "s#^.#~a#" name))
                          :output *standard-output* :error *error-output*)
    out-path))
;;;; END HACK

;;;; HACK QUICKDIST FOR RADIANCE
;; We have to do this in order to make our
;; Extrawurst handling of asdf compound
;; dependencies work.
(defun resolve-dependency-form (form)
  (cond
    ((listp form)
     (ecase (first form)
       (:version (second form))
       ;; interfaces are strictly skipped
       ;; as they're virtual components
       ;; that only make sense in a context
       ;; where Radiance already exists.
       (:interface NIL)))
    (t form)))

(defun quickdist::get-systems (asd-path)
  (with-open-file (s asd-path)
    (let* ((package (make-package (symbol-name (gensym "TMPPKG")) :use '(:cl :asdf)))
           (*package* package))
      (unwind-protect
           (sort
            (loop for form = (quickdist-reader:safe-read s nil)
                  while form
                  when (and (symbolp (car form))
                            (equalp "defsystem" (symbol-name (car form))))
                  collect (list* (cadr form)
                                 (sort (loop for dependency in (append (getf form :defsystem-depends-on)
                                                                       (getf form :depends-on))
                                             for name = (resolve-dependency-form dependency)
                                             when name collect name)
                                       #'string-lessp)))
            #'string-lessp :key #'first)
        (delete-package package)))))
;;;; END HACK

(defun redist ()
  "Creates a new dist by first REDRAWing all sources and then invoking QUICKDIST to create a new release."
  (let ((time (get-internal-real-time)))
    (format T "~&>> REDIST!~%")
    (format T "~&> Redrawing sources ...~%")
    (redraw)
    (format T "~&> Recreating dist ...~%")
    (quickdist:quickdist
     :name *dist-name*
     :version (timestamp)
     :base-url *dist-url*
     :projects-dir *sources-dir*
     :dists-dir *releases-dir*)
    (format T "~&>> DONE! (Took ~fs)~%"
            (/ (- (get-internal-real-time) time) internal-time-units-per-second))))

(defun purge-all-releases ()
  "Purges all stored releases. [INTERACTIVE]"
  (ask-purge-directory *releases-dir*))

(defun start-dist-server ()
  "Starts the dist server that will serve dist releases if none is active."
  (unless *acceptor*
    (pushnew *dispatcher* hunchentoot:*dispatch-table*)
    (hunchentoot:start (setf *acceptor* (make-instance 'hunchentoot:easy-acceptor :port *server-port*)))
    T))

(defun stop-dist-server ()
  "Stops the dist server if one is active."
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* NIL)
    T))
