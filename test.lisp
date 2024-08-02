(in-package #:org.shirakumo.redist)

(defvar *default-runner*)
(defvar *default-checkout-directory* NIL)
(defvar *default-report-directory* NIL)
(defvar *default-cache-directory* NIL)
(defvar *additional-source-directories* '(#p"~/common-lisp/"))

(define-condition test-failure (error)
  ((system :initarg :system :initform NIL :reader system)
   (tester :initarg :tester :initform NIL :reader tester)
   (report :initarg :report :initform NIL :reader report))
  (:report (lambda (c s) (format s "Failed to test~@[ ~a~]~@[ with ~a~]~@[:~%~%~a~]"
                                 (name (system c)) (tester c) (report c)))))

(defun checkout-directory (&rest dirs)
  (apply #'pathname-utils:subdirectory
         (or *default-checkout-directory*
             (pathname-utils:subdirectory (storage-file) "checkouts"))
         (mapcar #'string dirs)))

(defun report-directory (&rest dirs)
  (apply #'pathname-utils:subdirectory
         (or *default-report-directory*
             (pathname-utils:subdirectory (storage-file) "reports"))
         (mapcar #'string dirs)))

(defun cache-directory (&rest dirs)
  (apply #'pathname-utils:subdirectory
         (or *default-cache-directory*
             (pathname-utils:subdirectory (storage-file) "asdf-cache"))
         (mapcar #'string dirs)))

(defclass test ()
  ((results :initform (make-hash-table :test 'eq) :accessor results)
   (runner :initform *default-runner* :accessor runner)))

(defgeneric emit-test-result (test system result report))
(defgeneric test (test project &key))

(defmethod emit-test-result ((test test) (system system) result report)
  (setf (gethash system (results test)) result))

(defmethod test ((test (eql T)) thing &rest args &key &allow-other-keys)
  (apply #'test (make-instance 'reporting-test) thing args))

(defmethod test ((test test) (project project) &rest args &key &allow-other-keys)
  (do-list* (system (systems project))
    (with-simple-restart (continue "Ignore the failing system.")
      (apply #'test test system args))))

(defmethod test ((test test) (project project-release) &rest args &key &allow-other-keys)
  (do-list* (system (systems project))
    (with-simple-restart (continue "Ignore the failing system.")
      (apply #'test test system args))))

(defmethod test ((test test) (release release) &rest args &key (checkout-directory (checkout-directory (name (dist release)) (version release)))
                                                                   (cache-directory (cache-directory (name (dist release)) (version release)))
                                                                   verbose
                 &allow-other-keys)
  (when verbose (verbose "Ensuring checkout in ~a" checkout-directory))
  (do-list* (project (projects release))
    (when verbose (verbose "Checking out ~a" (name project)))
    (checkout project (pathname-utils:subdirectory checkout-directory (name project))))
  (do-list* (project (projects release))
    (with-simple-restart (continue "Ignore the failing project.")
      (apply #'test test project :checkout-directory checkout-directory :cache-directory cache-directory args))))

(defmethod test ((test test) (dist dist) &rest args &key &allow-other-keys)
  (apply #'test test (make-instance 'release :dist dist :version (next-version dist)) args))

(defmethod test ((test test) (name string) &rest args &key &allow-other-keys)
  (apply #'test test (dist name) args))

(defmethod test ((test test) (name symbol) &rest args &key &allow-other-keys)
  (apply #'test test (dist name) args))

(defmethod test ((test test) (system system) &rest args &key &allow-other-keys)
  (tagbody retry
     (restart-case 
         (handler-bind ((test-failure
                          (lambda (e)
                            (setf (slot-value e 'system) system)
                            (setf (slot-value e 'tester) test)
                            (emit-test-result test system :failed (report e))))
                        (error
                          (lambda (e)
                            (emit-test-result test system :errored (princ-to-string e)))))
           (let ((report (apply #'test (runner test) system args)))
             (emit-test-result test system :passed report)
             report))
       (retry ()
         :report "Retry testing the system."
         (go retry))
       (continue ()
         :report "Ignore the failing system."
         NIL))))

(defclass reporting-test (test)
  ((dir :initarg :dir :initform NIL :accessor dir)))

(defmethod test :before ((test reporting-test) (release release) &key)
  (unless (dir test)
    (setf (dir test) (pathname-utils:subdirectory (report-directory) (version release))))
  (ensure-directories-exist (dir test)))

(defmethod emit-test-result :after ((test reporting-test) (system system) result report)
  (generate-html (dir test) (name system) "report"
                 :runner (runner test)
                 :system system
                 :result result
                 :report report))

(defmethod emit-test-result ((test reporting-test) (release release) result report)
  (let ((reports (sort (loop for system being the hash-keys of (results test) using (hash-value result)
                             collect (list :system system
                                           :url (format NIL "~a.html" (name system))
                                           :result result))
                       #'string< :key (lambda (e) (name (getf e :system))))))
    (generate-html (dir test) "index" "release-report"
                   :runner (runner test)
                   :release release
                   :reports reports)))

(defmethod test :after ((test test) (release release) &key)
  (emit-test-result test release T T))

(defclass runner ()
  ((description :initarg :description :accessor description)))

(defmethod initialize-instance :after ((runner runner) &key)
  (setf (description runner)
        (apply #'run-string (program runner) 
               (eval-arguments runner '(format T "~a ~a on ~a ~a ~a"
                                        (lisp-implementation-type) (lisp-implementation-version)
                                        (machine-type) (software-type) (software-version))))))

(defgeneric program (runner))
(defgeneric load-arguments (runner file))
(defgeneric eval-arguments (runner form))

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

(defmethod test ((runner runner) (system system) &rest args &key verbose &allow-other-keys)
  (uiop:with-temporary-file (:pathname file :prefix (string (type-of runner)) :suffix (name system) :type "lisp")
    (apply #'write-test-file file system args)
    (test runner file :verbose verbose)))

(defmethod test ((runner runner) (file pathname) &key verbose)
  (let* ((output (make-string-output-stream))
         (target (if verbose (make-broadcast-stream output *standard-output*) output)))
    (if (< 0 (simple-inferiors:run (program runner) (load-arguments runner (pathname-utils:native-namestring file))
                                   :output target :error target))
        (error 'test-failure :report (get-output-stream-string output))
        (get-output-stream-string output))))

(defclass sbcl (runner)
  ())

(defmethod program ((runner sbcl))
  #+windows "sbcl.exe"
  #-windows "sbcl")

(defmethod load-arguments ((runner sbcl) file)
  (list "--dynamic-space-size" "8Gb" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
        "--no-sysinit" "--no-userinit" "--disable-debugger"
        "--load" (uiop:native-namestring file) "--quit"))

(defmethod eval-arguments ((runner sbcl) form)
  (list "--dynamic-space-size" "8Gb" "--noinform" "--disable-ldb" "--lose-on-corruption" "--end-runtime-options"
        "--no-sysinit" "--no-userinit" "--disable-debugger"
        "--eval" (with-standard-io-syntax (prin1-to-string form)) "--quit"))

(unless (boundp '*default-runner*)
  (setf *default-runner* (make-instance 'sbcl)))
