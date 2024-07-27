(in-package #:org.shirakumo.redist)

(define-condition test-failure (error)
  ((system :initarg :system :reader system)
   (tester :initarg :tester :reader tester)
   (report :initarg :report :initform NIL :reader report))
  (:report (lambda (c s) (format s "Failed to test ~a with ~a~@[:~%~%~a~]"
                                 (name (system c)) (tester c) (report c)))))

(defvar *default-tester*)

(defclass tester ()
  ())

(defgeneric test (tester project &key))

(defmethod test ((tester (eql T)) thing &rest args &key &allow-other-keys)
  (apply #'test *default-tester* thing args))

(defmethod test ((tester tester) (project project) &rest args &key &allow-other-keys)
  (do-list* (system (systems project))
    (with-simple-restart (continue "Ignore the failing system.")
      (apply #'test tester system args))))

(defmethod test ((tester tester) (project project-release) &rest args &key &allow-other-keys)
  (do-list* (system (systems project))
    (with-simple-restart (continue "Ignore the failing system.")
      (apply #'test tester system args))))

(defmethod test ((tester tester) (release release) &rest args &key &allow-other-keys)
  (do-list* (project (projects release))
    (with-simple-restart (continue "Ignore the failing project.")
      (apply #'test tester project args))))

(defmethod test ((tester tester) (dist dist) &rest args &key &allow-other-keys)
  (do-list* (project (projects dist))
    (when (active-p project)
      (restart-case (apply #'test tester project args)
        (deactivate ()
          :report "Deactivate the project and continue."
          (setf (active-p project) NIL))
        (continue (&optional e)
          :report "Skip the project and continue."
          (declare (ignore e))
          NIL)))))

(defclass program-tester (tester)
  ())

(defgeneric program (program-tester))
(defgeneric load-arguments (program-tester file))

(defun write-test-file (file system &key (source-directory #p "~/dist/sources/") (cache-directory #p "~/dist/asdf-cache/") run-tests verbose)
  (when verbose
    (verbose "Writing test file for ~a to ~a" (name system) file))
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (dolist (form `((require :asdf)
                      (asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration (:tree ,(pathname-utils:native-namestring source-directory))))
                      (asdf:initialize-output-translations '(:output-translations :ignore-inherited-configuration (T (,(pathname-utils:native-namestring cache-directory) :implementation ,(name system)))))
                      (asdf:load-system ',(name system))
                      ,@(when run-tests
                          `((setf cl-user::*exit-on-test-failures* T) ; Not standardised.
                            (asdf:test-system ',(name system))))))
        (pprint form stream)
        (terpri stream)))))

(defmethod test ((tester program-tester) (system system) &rest args &key verbose)
  (uiop:with-temporary-file (:pathname file :prefix (type-of tester) :suffix (name system) :type "lisp")
    (apply #'write-test-file file system args)
    (handler-case (test tester file :verbose verbose)
      (test-failure (c)
        (error 'test-failure :tester tester :system system :report (report c))))))

(defmethod test ((tester program-tester) (file pathname) &key verbose)
  (let* ((output (make-string-output-stream))
         (target (if verbose (make-broadcast-stream output *standard-output*) output)))
    (when (< 0 (simple-inferiors:run (program tester) (load-arguments tester (pathname-utils:native-namestring file))
                                     :output target :error target))
      (error 'test-failure :tester tester :report (get-output-stream-string output)))))

(defclass sbcl (tester)
  ())

(defmethod program ((sbcl sbcl))
  #+windows "sbcl.exe"
  #-windows "sbcl")

(defmethod load-arguments ((sbcl sbcl) file)
  (list "--dynamic-space-size" "8Gb" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
        "--no-sysinit" "--no-userinit" "--disable-debugger"
        "--load" (uiop:native-namestring file) "--quit"))

(unless (boundp '*default-tester*)
  (setf *default-tester* (make-instance 'sbcl)))
