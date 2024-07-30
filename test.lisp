(in-package #:org.shirakumo.redist)

(defvar *default-tester*)
(defvar *default-checkout-directory* NIL)
(defvar *default-report-directory* NIL)
(defvar *default-cache-directory* NIL)
(defvar *additional-source-directories* '(#p"~/common-lisp/"))

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

(defun report-directory (&rest dirs)
  (apply #'pathname-utils:subdirectory
         (or *default-report-directory*
             (pathname-utils:subdirectory (storage-file) "reports"))
         dirs))

(defun cache-directory (&rest dirs)
  (apply #'pathname-utils:subdirectory
         (or *default-cache-directory*
             (pathname-utils:subdirectory (storage-file) "asdf-cache"))
         dirs))

(defclass tester ()
  ((results :initform (make-hash-table :test 'eq) :accessor results)))

(defgeneric emit-test-result (tester system result report))
(defgeneric test (tester project &key))
(defgeneric runner (tester))

(defmethod emit-test-result ((tester tester) (system system) result report)
  (setf (gethash system (results tester)) result))

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

(defmethod test ((tester tester) (dist dist) &rest args &key &allow-other-keys)
  (apply #'test tester (make-instance 'release :dist dist :version (next-version dist)) args))

(defmethod test ((tester tester) (name string) &rest args &key &allow-other-keys)
  (apply #'test tester (dist name) args))

(defmethod test :around ((tester tester) (system system) &key)
  (tagbody retry
     (restart-case (return-from test (call-next-method))
       (retry ()
         :report "Retry testing the system."
         (go retry)))))

(defclass reporting-tester (tester)
  ((dir :initarg :dir :initform (report-directory) :accessor dir)))

(defmethod emit-test-result :after ((tester reporting-tester) (system system) result report)
  (generate-html (dir tester) (name system) "report"
                 :runner (runner tester)
                 :system system
                 :result result
                 :report report))

(defmethod emit-test-result ((tester reporting-tester) (dist dist) result report)
  ())

(defmethod emit-test-result ((tester reporting-tester) (release release) result report)
  ())

(defmethod test :after ((tester tester) (dist dist) &key)
  (emit-test-result tester dist T T))

(defmethod test :after ((tester tester) (release release) &key)
  (emit-test-result tester release T T))

(defclass program-tester (tester)
  ((runner :accessor runner)))

(defmethod initialize-instance :after ((tester program-tester) &key)
  (setf (runner tester) (apply #'run-string (program tester) 
                               (eval-arguments tester '(format T "~a ~a on ~a ~a ~a"
                                                        (lisp-implementation-type) (lisp-implementation-version)
                                                        (machine-type) (software-type) (software-version))))))

(defgeneric program (program-tester))
(defgeneric load-arguments (program-tester file))
(defgeneric eval-arguments (program-tester form))

(defun write-test-file (file system &key checkout-directory cache-directory run-tests verbose)
  (when verbose
    (verbose "Writing test file for ~a to ~a" (name system) file))
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (dolist (form `((require :asdf)
                      ,@(when checkout-directory
                          `((asdf:initialize-source-registry '(:source-registry :ignore-inherited-configuration
                                                               ,@(loop for dir in (enlist checkout-directory)
                                                                       collect `(:tree ,(pathname-utils:native-namestring dir)))
                                                               ,@(loop for dir in *additional-source-directories*
                                                                       collect `(:tree ,(pathname-utils:native-namestring dir)))))))
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
    (handler-case (emit-test-result tester system :passed (test tester file :verbose verbose))
      (test-failure (c)
        (emit-test-result tester system :failed (report c))
        (error 'test-failure :tester tester :system system :report (report c))))))

(defmethod test ((tester program-tester) (file pathname) &key verbose)
  (let* ((output (make-string-output-stream))
         (target (if verbose (make-broadcast-stream output *standard-output*) output)))
    (if (< 0 (simple-inferiors:run (program tester) (load-arguments tester (pathname-utils:native-namestring file))
                                   :output target :error target))
        (error 'test-failure :tester tester :report (get-output-stream-string output))
        (get-output-stream-string output))))

(defclass sbcl (program-tester)
  ())

(defmethod program ((tester sbcl))
  #+windows "sbcl.exe"
  #-windows "sbcl")

(defmethod load-arguments ((tester sbcl) file)
  (list "--dynamic-space-size" "8Gb" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
        "--no-sysinit" "--no-userinit" "--disable-debugger"
        "--load" (uiop:native-namestring file) "--quit"))

(defmethod eval-arguments ((tester sbcl) form)
  (list "--dynamic-space-size" "8Gb" "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
        "--no-sysinit" "--no-userinit" "--disable-debugger"
        "--eval" (with-standard-io-syntax (prin1-to-string form)) "--quit"))

(unless (boundp '*default-tester*)
  (setf *default-tester* (make-instance 'sbcl)))
