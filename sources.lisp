#|
 This file is a part of Shirakumo-Dist
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defvar *repositories* ()
  "List of all GIT repository URLs.")

(defvar *repositories-file*
  (asdf:system-relative-pathname :shirakumo-dist (make-pathname :name "repositories" :type "txt"))
  "File containing a list of repository URLs.
The format of the file is simple; each line is a repository URL.
Spaces are permitted and empty lines or lines starting with a 
semicolon are ignored.")

(defvar *sources-dir*
  (asdf:system-relative-pathname :shirakumo-dist "sources" :type :directory)
  "The directory pathname to where the sources from which the dists are built should be stored.")

(defun load-repositories ()
  "Load the list of repositories from the file.

See *repositories-file*"
  (when (uiop:file-exists-p *repositories-file*)
    (with-open-file (stream *repositories-file* :direction :input)
      (loop for line = (read-line stream NIL NIL)
            while line
            for sparse-line = (string-trim '(#\Space #\Newline #\Tab #\Return #\Linefeed #\Page) line)
            when (and (string/= "" line)
                      (not (char= (aref sparse-line 0) #\;)))
            do (add-repository sparse-line :dump NIL)))))

(defun dump-repositories ()
  "Saves the list of *repositories* to the *repositories-file*."
  (with-open-file (stream *repositories-file* :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line (format NIL ";; Repository dump generated on ~a" (timestamp)) stream)
    (loop for repository in (sort (copy-list *repositories*) #'string<)
          do (write-line repository stream))
    *repositories*))

(defun add-repository (repository &key (dump T))
  "Adds repository to the list of repositories if it does not yet exist.

See dump-repositories."
  (pushnew (string repository) *repositories* :test #'string-equal)
  (when dump (dump-repositories))
  *repositories*)

(defun remove-repository (repository &key (dump T) (test #'search*))
  "Removes repository from the list of repositories if it matches by the given test.

See search*
See dump-repositories."
  (setf *repositories*
        (delete (string repository) *repositories* :test test))
  (when dump (dump-repositories))
  *repositories*)

(defun have-repository (repository &key (test #'search*))
  "Returns the repository if it has been found in the list by test and NIL otherwise."
  (find repository *repositories* :test test))

(defun repository-name (repository)
  "Returns the determined name of the repository.
E.g: https://github.com/example/com.git => com
     https://github.com/example/com     => com"
  (let ((last-slash (position #\/ repository :from-end T))
        (last-dot (position #\. repository :from-end T)))
    (if (and last-dot (< last-slash last-dot))
        (subseq repository (1+ last-slash) last-dot)
        (subseq repository (1+ last-slash)))))

(defun repository-directory (repository)
  "Returns the directory pathname in which the repository is stored.

See *sources-dir*
See repository-name"
  (merge-pathnames (format NIL "~a/" (repository-name repository)) *sources-dir*))

(defun repository-exists-p (repository)
  "Returns T if the repository's directory exists.

See repository-directory"
  (uiop:directory-exists-p (repository-directory repository)))

(defun create-repository (repository)
  "Effectively creates the repository by cloning it.

See git-clone
See repository-directory"
  (git-clone (repository-directory repository) repository :depth 1 :submodules T)
  T)

(defun redraw-repository (repository)
  "Effectively redraws the repository by pulling it.

See git-pull
See repository-directory"
  (git-pull (repository-directory repository) :submodules T)
  T)

(defun ensure-repository-ready (repository)
  "Ensures that the given repository exists and is up-to-date.

See repository-exists-p
See redraw-repository
See create-repository"
  (if (repository-exists-p repository)
      (progn (redraw-repository repository) :redraw)
      (progn (create-repository repository) :create)))

(defun purge-unknown-repositories ()
  "Removes all the repository directories under *sources-dir* that aren't
found in the local *repositories* list by have-repository."
  (loop for dir in (uiop:subdirectories *sources-dir*)
        when (not (have-repository (car (last (pathname-directory dir)))))
        do (uiop:delete-directory-tree dir :validate (constantly T))))

(defun purge-all-sources ()
  "Purges all stored sources. [INTERACTIVE]"
  (ask-purge-directory *sources-dir*))

(defun ensure-repositories-ready ()
  "Makes sure all the repositories are available and up-to-date.

See ensure-repository-read
See *repositories*"
  (mapc #'ensure-repository-ready *repositories*)
  T)

(defun redraw ()
  "Loads the repository list from disk, purges unknown repositories and then ensures everything is up-to-date.

See load-repositories
See purge-unknown-repositories
See ensure-repositories-ready"
  (load-repositories)
  (purge-unknown-repositories)
  (ensure-repositories-ready)
  T)
