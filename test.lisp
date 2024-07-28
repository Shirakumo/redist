(in-package #:org.shirakumo.redist)

(defvar *default-tester*)
(defvar *default-checkout-directory* NIL)
(defvar *default-cache-directory* NIL)

(define-condition test-failure (error)
  ((system :initarg :system :reader system)
   (tester :initarg :tester :reader tester)
   (report :initarg :report :initform NIL :reader report))
  (:report (lambda (c s) (format s "Failed to test ~a with ~a~@[:~%~%~a~]"
                                 (name (system c)) (tester c) (report c)))))

(defun checkout-directory (&rest dirs)
  (apply #'pathname-utils:subdirectory
         (or *default-checkout-directory*
             (pathname-utils:subdirectory (storage-file) "checkouts"))
         dirs))

(defun cache-directory (&rest dirs)
  (apply #'pathname-utils:subdirectory
         (or *default-cache-directory*
             (pathname-utils:subdirectory (storage-file) "asdf-cache"))
         dirs))

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

(defmethod test ((tester tester) (release release) &rest args &key (checkout-directory (checkout-directory (name (dist release)) (version release)))
                                                                   (cache-directory (cache-directory (name (dist release)) (version release)))
                                                                   verbose
                 &allow-other-keys)
  (when verbose (verbose "Ensuring checkout in ~a" checkout-directory))
  (do-list* (project (projects release))
    (when verbose (verbose "Checking out ~a" (name project)))
    (checkout project (pathname-utils:subdirectory checkout-directory (name project))))
  (do-list* (project (projects release))
    (with-simple-restart (continue "Ignore the failing project.")
      (apply #'test tester project :checkout-directory checkout-directory :cache-directory cache-directory args))))

(defmethod test ((tester tester) (dist dist) &rest args &key (checkout-directory (checkout-directory (name dist) "current"))
                                                             (cache-directory (cache-directory (name dist) "current"))
                                                             verbose
                 &allow-other-keys)
  (when verbose (verbose "Ensuring checkout in ~a" checkout-directory))
  (do-list* (project (projects dist))
    (when verbose (verbose "Checking out ~a" (name project)))
    (checkout project (pathname-utils:subdirectory checkout-directory (name project))))
  (do-list* (project (projects dist))
    (unless (disabled-p project)
      (restart-case (apply #'test tester project :checkout-directory checkout-directory :cache-directory cache-directory args)
        (disable ()
          :report "Disable the project and continue."
          (setf (disabled-p project) T))
        (continue (&optional e)
          :report "Skip the project and continue."
          (declare (ignore e))
          NIL)))))

(defmethod test ((tester tester) (name string) &rest args &key &allow-other-keys)
  (apply #'test tester (dist name) args))

(defclass program-tester (tester)
  ())

(defgeneric program (program-tester))
(defgeneric load-arguments (program-tester file))

(defun write-test-file (file system &key checkout-directory cache-directory run-tests verbose)
  (when verbose
    (verbose "Writing test file for ~a to ~a" (name system) file))
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (dolist (form `((require :asdf)
                      ,@(when checkout-directory
                          `((asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration (:tree ,(pathname-utils:native-namestring checkout-directory))))))
                      ,@(when cache-directory
                          `((asdf:initialize-output-translations '(:output-translations :ignore-inherited-configuration (T (,(pathname-utils:native-namestring cache-directory) :implementation))))))
                      (asdf:load-system ',(name system))
                      ,@(when run-tests
                          `((setf cl-user::*exit-on-test-failures* T) ; Not standardised.
                            (asdf:test-system ',(name system))))))
        (pprint form stream)
        (terpri stream)))))

(defmethod test ((tester program-tester) (system system) &rest args &key verbose &allow-other-keys)
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

(defclass sbcl (program-tester)
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
