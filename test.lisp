#|
 This file is a part of Shirakumo-Dist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defvar *default-tester*)

(defclass tester ()
  ())

(defgeneric test (tester project &key))

(defmethod test ((tester (eql T)) thing &rest args &key &allow-other-keys)
  (apply #'test *default-tester* thing args))

(defmethod test ((tester tester) (project project) &rest args &key &allow-other-keys)
  (dolist (system (systems project))
    (with-simple-restart (continue "Ignore the failing system.")
      (apply #'test tester system args))))

(defmethod test ((tester tester) (project project-release) &rest args &key &allow-other-keys)
  (dolist (system (systems project))
    (with-simple-restart (continue "Ignore the failing system.")
      (apply #'test tester system args))))

(defmethod test ((tester tester) (release release) &rest args &key &allow-other-keys)
  (dolist (project (projects release))
    (with-simple-restart (continue "Ignore the failing project.")
      (apply #'test tester project args))))

(defmethod test ((tester tester) (dist dist) &rest args &key &allow-other-keys)
  (dolist (project (projects dist))
    (when (active-p project)
      (restart-case (apply #'test tester project args)
        (deactivate ()
          :report "Deactivate the project and continue."
          (setf (active-p project) NIL))
        (continue (&optional e)
          :report "Skip the project and continue."
          (declare (ignore e))
          NIL)))))

(defclass sbcl (tester)
  ())

(defun form-string (form)
  (with-standard-io-syntax
    (prin1-to-string form)))

(defmethod test ((tester sbcl) (system system) &key (source-directory #p "~/dist/sources/") (cache-directory #p "~/dist/cache/") verbose run-tests)
  (let* ((output (make-string-output-stream))
         (target (if verbose (make-broadcast-stream output *standard-output*) output)))
    (when (< 0 (simple-inferiors:run #+windows "sbcl.exe" #-windows "sbcl"
                                     (list "--dynamic-space-size" "8Gb" "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
                                           "--no-sysinit" "--no-userinit" "--disable-debugger"
                                           "--eval" "(require :asdf)"
                                           "--eval" (form-string `(asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration (:tree ,(namestring source-directory)))))
                                           "--eval" (form-string `(asdf:initialize-output-translations '(:output-translations :ignore-inherited-configuration (T (,(namestring cache-directory) :implementation)))))
                                           "--eval" (form-string `(asdf:load-system ',(name system)))
                                           "--eval" (form-string `(setf cl-user::*exit-on-test-failures* T)) ; Not standardised.
                                           ;; FIXME: How do we determine test alias systems from regulars? We don't want to test the same thing twice.
                                           "--eval" (if run-tests (form-string `(asdf:test-system ',(name system))) "()")
                                           "--quit")
                                     :output target :error target))
      (error "Testing~%  ~a~%failed:~%~%~a" system (get-output-stream-string output)))))

(unless (boundp '*default-tester*)
  (setf *default-tester* (make-instance 'sbcl)))
