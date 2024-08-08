(in-package #:org.shirakumo.redist)

(defun envvar (var)
  (let ((val (uiop:getenv var)))
    (when (and val (string/= "" val))
      val)))

(defmacro with-envvar ((val var) &body body)
  `(let ((,val (envvar ,var)))
     (when ,val ,@body)))

(defun create-update-script (&key (file (merge-pathnames "redist.lisp" (storage-file))) (if-exists :supersede))
  (ensure-directories-exist file)
  (with-open-file (stream file :direction :output :if-exists if-exists)
    (with-standard-io-syntax
      (let ((*package* #.*package*)
            (*print-case* :downcase)
            (*print-right-margin* 80)
            (*print-readably* NIL))
        (format stream "~
#| Dist update script compiled automatically
exec ~a --noinform --load \"$0\" -- \"$@\"
#|
#-quicklisp (load ~s)
(ql:quickload :redist :silent T)
(setf org.shirakumo.redist:*storage-file* ~s)
(setf org.shirakumo.redist:*default-output-directory* ~s)
(setf org.shirakumo.redist:*default-source-directory* ~s)
(org.shirakumo.redist:main)"
                (pathname-utils:native-namestring (truename (first (uiop:raw-command-line-arguments))))
                (ql-impl-util::quicklisp-init-file-form)
                (storage-file)
                (default-output-directory)
                (default-source-directory)))))
  file)

(defun self ()
  (pathname-utils:native-namestring (truename (first (uiop:raw-command-line-arguments)))))

(defun create-systemd-service (&key (file "/etc/systemd/system/redist.service") (if-exists :supersede) (binary (self)) (interval "monthly") enable)
  (with-open-file (stream file :direction :output :if-exists if-exists)
    (format stream "~
[Unit]
Description=Redist Distribution Compilation
After=network.target

[Service]
Type=oneshot
ExecStart=~a compile -vuj 4
WorkingDirectory=~a
User=~a
Group=~a
Environment=DIST_SOURCE_DIR=~a
Environment=DIST_OUTPUT_DIR=~a
Environment=STORAGE_FILE=~a"
            binary
            (pathname-utils:native-namestring (uiop:getcwd))
            #-sbcl "redist" #+sbcl (sb-posix:passwd-name (sb-posix:getpwuid (sb-posix:stat-uid (sb-posix:stat binary))))
            #-sbcl "redist" #+sbcl (sb-posix:group-name (sb-posix:getgrgid (sb-posix:stat-uid (sb-posix:stat binary))))
            (pathname-utils:native-namestring (default-source-directory))
            (pathname-utils:native-namestring (default-output-directory))
            (pathname-utils:native-namestring (storage-file))))
  (with-open-file (stream (make-pathname :type "timer" :defaults file) :direction :output :if-exists if-exists)
    (format stream "~
[Unit]
Description=Redist Releases

[Timer]
OnCalendar=~a
Persistent=true

[Install]
WantedBy=timers.target"
            interval))
  (when enable
    (uiop:run-program (list "systemctl" "enable" "redist.timer")))
  file)

(defun main/help ()
  (format T "Commands:

compile               Compile a dist release
     --version version   Specify the version to compile
  -d --dist dist         Specify the dist to compile. Can be specified
                         multiple times. If unspecified, all dists are
                         compiled
  -u --update            If specified will update associated projects
                         first
  -v --verbose           If specified shows updates about the progress
  -f --force             If specified compiles the dist version even
                         if it exists already
  -x --overwrite         Overwrite the last release. Cannot be used
                         together with version. Implies --force
  -j --jobs N            Make the compilation threaded.

update                Update local project checkouts
     --version version   Specify the version to update to. If
                         unspecified, updates to latest
  -p --project project   Specify the project to update. Can be
                         specified multiple times. If unspecified, all
                         projects are updated
  -v --verbose           If specified shows updates about the progress
  -j --jobs              Make the compilation threaded.

list                  List known objects
  thing                  The thing to list. Can be projects, dists, or
                         releases. Defaults to releases of all dists.
  -p --project project   The project to list releases of
  -d --dist dist         The dist to list releases of

describe              Describe information about an object
  -p --project project   The project to describe
  -d --dist dist         The dist to describe
     --version version   The version to describe

add                   Add a new project or add a project to a dist
  url                    The url of the project's primary remote.
  -t --type type         The type of the remote source. Defaults to
                         GIT
  -n --name name         The name of the project. If unspecified is
                         derived from the url
  -d --dist dist         The dist to add the project to. Can be
                         specified multiple times. If unspecified,
                         adds to all dists

add-dist              Add a new dist
  name                   The name of the dist to create
  --url url              The canonical URL at which the dist resides

remove                Remove a project from dists
  name                   The name of the project to remove
  -d --dist dist         The dist to remove the project from. Can be
                         specified multiple times. If unspecified,
                         removes from all dists

replicate             Create a new mirror of another dist
  url                    The URL of the distinfo file to replicate
  -n --name name         The name of the dist. If unspecified is
                         taken from the dist metadata.
  -v --verbose           To print verbose output about the progress
  -l --latest-only       To only download the latest version, rather
                         than the default of all available versions
  -s --skip-archives     Don't download the release archives

archive               Update the release archives
  -p --project project   Update all archives of the project
  -d --dist dist         Update all archives of projects in the dist
     --version version   Update only the given version
  -v --verbose           To print verbose output about the progress

test                  Test a dist release
  name                   The name of the dist to test a release of
  -u --update            If specified will update associated projects
                         first and create a new release to run the
                         tests on. The release won't be published.
  -v --verbose           If specified shows the output of the tests
  -j --jobs N            Make the testing threaded. This does *not*
                         pair well with -v.

install               Install a Systemd service. Requires root.
  -i --interval interval Set the timer interval. Defaults to monthly
  -e --enable            Enable the service

help                  Shows this help listing

Environment Variables:

REDIST_DEBUG          When set, will enter the debugger on error
DIST_SOURCE_DIR       The base directory of the project sources. If
                      unspecified, defaults to
                      DISTINFO_FILE/sources/
DIST_OUTPUT_DIR       The base directory of the compiled
                      output. You'll want to point your webserver at
                      this. If unspecified, defaults to
                      DISTINFO_FILE/releases/
STORAGE_FILE          The file that contains all dist storage info.
                      If unspecified tries in order:
                      DIST_SOURCE_DIR/../distinfo.db
                      DIST_SOURCE_DIR/../distinfo.sexp
                      DIST_OUTPUT_DIR/../distinfo.db
                      DIST_OUTPUT_DIR/../distinfo.sexp
                      ~~/dist/distinfo.db
                      ~~/dist/distinfo.sexp
                      distinfo.db
                      distinfo.sexp

Database Info:

Redist keeps a database of all the dists, projects, and releases. This
database can be stored as plaintext or as an sqlite database. This is
informed by the STORAGE_FILE. Redist loads this on startup, and will
persist any changes made during your operations to it again. If no
database is present, a basic plaintext storage is created for you.

Please see https://shirakumo.org/projects/redist for more information.
"))

(defun main/compile (&key version update dist verbose force overwrite jobs)
  (when overwrite
    (when version (error "Cannot specify version alongside overwrite."))
    (unless force (setf force T)))
  (let ((args (list* :if-exists :supersede :force force :update update :verbose verbose
                     (if version (list :version version)))))
    (with-kernel (when jobs (parse-integer jobs))
      (do-list* (dist (or (enlist dist) (list-dists)))
        (if overwrite
            (apply #'compile dist :version (version (first (releases dist))) args)
            (apply #'compile dist args))))))

(defun main/update (&key version project verbose jobs)
  (with-kernel (when jobs (parse-integer jobs))
    (do-list* (project (or (enlist project) (list-projects)))
      (unless (disabled-p project)
        (update project :version version :verbose verbose)))))

(defun main/list (&optional (thing "releases") &key project dist)
  (cond ((string-equal thing "projects")
         (dolist (project (list-projects))
           (format T "~&~a~%" (name project))))
        ((string-equal thing "dists")
         (dolist (dist (list-dists))
           (format T "~&~a~%" (name dist))))
        ((or (string-equal thing "releases")
             (string-equal thing "versions"))
         (dolist (release (cond (project (releases (project project)))
                                (dist (releases (dist dist)))
                                (T (loop for dist in (list-dists) append (releases dist)))))
           (format T "~&~a~%" (version release))))
        ((string-equal thing "sources")
         (labels ((rec (class)
                    (dolist (class (c2mop:class-direct-subclasses class))
                      (format T "~&~a~%" (class-name class))
                      (rec class))))
           (rec (find-class 'source-manager))))
        (T
         (error "Don't know how to list ~a." thing))))

(defun main/describe (&key dist project version)
  (cond (dist
         (let ((dist (or (dist dist) (error "No dist named ~s" dist))))
           (describe (if version
                         (find-release version dist)
                         dist))))
        (project
         (let ((project (or (project project) (error "No project named ~s" project))))
           (describe (if version
                         (find-release version project)
                         project))))
        (T
         (format *error-output* "~
Sources:~12t~a
Output:~12t~a
Storage:~12t~a
Dists:"
                 (default-source-directory)
                 (default-output-directory)
                 (storage-file))
         (loop for dist in (list-dists)
               do (format *error-output* "~12t~a ~a~%" (version dist) (name dist)))
         (format *error-output* "Projects:")
         (loop for project in (list-projects)
               do (format *error-output* "~12t~a ~a~%" (version project) (name project))))))

(defun main/add (url &key type name dist)
  (let* ((name (or name (url-extract-name url)))
         (type (intern (string-upcase (or type "git")) "KEYWORD"))
         (project (or (project name)
                      (make-instance 'project :name name :sources `((,type ,url))))))
    (dolist (dist (or (enlist dist) (list-dists)) (setf (project name) project))
      (add-project project dist))))

(defun main/add-dist (name &key url)
  (let ((name (intern (string-upcase name) #.*package*)))
    (when (dist name)
      (error "A dist with this name already exists."))
    (when (or (null url) (string= "" url))
      (error "A canonical dist URL is required."))
    (setf (dist name) (make-instance 'dist :name name :url url))))

(defun main/remove (name &key dist)
  (let ((project (or (project name)
                     (error "No project named ~s" name))))
    (dolist (dist (or (enlist dist) (list-dists)))
      (remove-project project dist))))

(defun main/replicate (url &key name verbose latest-only skip-archives)
  (replicate-dist url :name name
                      :verbose verbose
                      :current-version-only latest-only
                      :download-archives (not skip-archives)))

(defun main/archive (&key project dist version verbose)
  (labels ((package (release)
             (compile release :force T :verbose verbose))
           (process (project)
             (if version
                 (let ((release (find-release version project)))
                   (when release (package release)))
                 (mapc #'package (releases project)))))
    (when project
      (process (or (project project)
                   (error "No project named ~s" project))))
    (when dist
      (mapc #'process (projects (or (dist dist)
                                    (error "No dist named ~s" dist)))))))

(defun main/install (&key enable (interval "monthly"))
  (create-systemd-service :enable enable :interval interval))

(defun main/test (dist &key update verbose jobs)
  (when update
    (with-kernel (when jobs (parse-integer jobs))
      (dolist (project (projects dist))
        (update project))))
  (with-kernel (when jobs (parse-integer jobs))
    (test T dist :verbose (if verbose :full T)
                 :on-error :continue
                 :use-latest-release (not update))))

(defun parse-args (args &key flags chars)
  (let ((kargs ())
        (pargs ()))
    (loop for arg = (pop args)
          while arg
          do (labels ((next-arg (arg)
                        (if args
                            (pop args)
                            (error "Missing value for ~a" arg)))
                      (handle-argument (arg)
                        (setf (getf kargs arg) (cond ((find arg flags :test #'string-equal)
                                                      T)
                                                     ((null (getf kargs arg))
                                                      (next-arg arg))
                                                     ((consp (getf kargs arg))
                                                      (list* (next-arg arg) (getf kargs arg)))
                                                     (T
                                                      (list (next-arg arg) (getf kargs arg)))))))
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
  #+sbcl (sb-ext:disable-debugger)
  (let ((args (or args '("help"))))
    (setf *here* (pathname-utils:to-directory (first (uiop:raw-command-line-arguments))))
    (handler-bind ((error
                     (lambda (e)
                       (cond ((uiop:getenv "REDIST_DEBUG")
                              #+sbcl (sb-ext:enable-debugger)
                              (invoke-debugger e))
                             (T
                              (format *error-output* "~&ERROR: ~a~%" e)
                              (lparallel:end-kernel)
                              (uiop:print-condition-backtrace e)
                              (uiop:quit 2)))))
                   (warning
                     (lambda (e)
                       (format *error-output* "~&WARNING: ~a~%" e)
                       (muffle-warning e)))
                   #+sbcl
                   (sb-sys:interactive-interrupt
                     (lambda (e)
                       (format *error-output* "~&Interactive interrupt~%")
                       (lparallel:end-kernel)
                       (uiop:quit 1))))
      (destructuring-bind (command . args) args
        (let ((cmdfun (find-symbol (format NIL "~a/~:@(~a~)" 'main command) #.*package*)))
          (unless cmdfun
            (error "No command named ~s." command))
          (with-envvar (val "DIST_SOURCE_DIR")
            (setf *default-source-directory* (pathname-utils:parse-native-namestring val :as :directory)))
          (with-envvar (val "DIST_OUTPUT_DIR")
            (setf *default-output-directory* (pathname-utils:parse-native-namestring val :as :directory)))
          (with-envvar (val "STORAGE_FILE")
            (setf *storage-file* (pathname-utils:parse-native-namestring val :as :file)))
          (try-open-storage)
          (apply #'funcall cmdfun (parse-args args :flags '(:verbose :update :force :overwrite :latest-only :skip-archives :enable)
                                                   :chars '(#\v :verbose #\u :update #\f :force
                                                            #\d :dist #\p :project #\n :version
                                                            #\n :name #\t :type #\x :overwrite
                                                            #\j :jobs #\l :latest-only
                                                            #\s :skip-archives
                                                            #\i :interval #\e :enable)))
          (when *storage* (store T T T)))))
    (uiop:quit)))

;; Sigh.
(cffi:close-foreign-library 'sqlite-ffi::sqlite3-lib)
