#|
 This file is a part of Shirakumo-Dist
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defvar *impl-command* '("sbcl"))
(defvar *bootstrapper* (asdf:system-relative-pathname :shirakumo-dist "test-bootstrap.lisp"))
(defvar *manifest-file* NIL)

(defun call-external-impl (quiet &rest args)
  (uiop:run-program
   (append *impl-command* args)
   :output (not quiet) :error-output (not quiet)))

(defun directory-name (pathname)
  (let ((dir (uiop:pathname-directory-pathname pathname)))
    (car (last (pathname-directory dir)))))

(defun generate-manifest (&key (dirs '(#p"~/quicklisp/local-projects/"
                                       #p"~/quicklisp/dists/quicklisp/software"))
                               (manifest *manifest-file*)
                               verbose)
  (with-open-file (stream manifest :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (dir dirs)
      (when verbose (format T "~&Processing ~a~%" dir))
      (let ((files ()))
        ;; Scan
        (labels ((scan (dir)
                   (dolist (dir (uiop:subdirectories dir))
                     (cond
                       ((string= (directory-name dir) ".git")
                        (when verbose (format T "~&Ignoring .GIT ~a ~%" dir)))
                       (T
                        (when verbose (format T "~&Descending into ~a~%" dir))
                        (scan dir))))
                   (dolist (file (uiop:directory-files dir))
                     (cond
                       ((string-equal (pathname-type file) "asd")
                        (when verbose (format T "~&Registering ~a~%" file))
                        (push (uiop:native-namestring file) files))))))
          (scan dir))
        ;; Sort and write
        (when verbose (format T "~&Writing to manifest for ~a~%~%" dir))
        (dolist (file (sort files #'string<))
          (write-line file stream)))))
  manifest)

(defun test-system-externally (system-name &key (manifest *manifest-file*) log-file (quiet T))
  (call-external-impl
   quiet
   "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
   "--no-sysinit" "--no-userinit" "--disable-debugger" 
   "--load" (uiop:native-namestring *bootstrapper*)
   "--eval" (format NIL "(!boot:test! ~s~@[ :manifest ~s~]~@[ :log-file ~s~])"
                    system-name manifest log-file)
   "--quit" "--end-toplevel-options")
  (or log-file system-name))

(defun test-batch (&rest systems)
  (let ((pass 0) (fail 0))
    (dolist (system systems)
      (format T "~&Testing system ~a ... " system)
      (finish-output)
      (if (ignore-errors (test-system-externally system))
          (progn (incf pass) (format T "[PASS]~%"))
          (progn (incf fail) (format T "[FAIL]~%"))))
    (format T "~&
Tally:
  ~d passed
  ~d failed" pass fail)))
