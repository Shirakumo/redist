(in-package #:org.shirakumo.redist)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-pathname* *load-pathname*)))

(defun tar (files output &key (if-exists :error) (base #p"/") (archive-root #p"") (verbose))
  (when verbose (verbose "Packing to ~a:~{~&;   ~a~}" output files))
  (archive:with-open-archive (archive output
                              :direction :output
                              :if-exists if-exists)
    (let* ((base (truename base))
           (*default-pathname-defaults* base))
      (dolist (file files)
        (with-simple-restart (continue "Ignore the failing file.")
          ;; SIGH: have to redo this here so that we can fake the archive root.
          (let* ((stat (archive::stat file))
                 (entry (make-instance 'archive::tar-entry
                                       :pathname (merge-pathnames (pathname-utils:enough-pathname file base) archive-root)
                                       :mode (logand archive::+permissions-mask+ (archive::stat-mode stat))
                                       :typeflag (archive::typeflag-for-mode (archive::stat-mode stat))
                                       :uid (archive::stat-uid stat)
                                       :gid (archive::stat-gid stat)
                                       :size (archive::stat-size stat)
                                       :mtime (archive::stat-mtime stat))))
            (when (pathname-utils:directory-p file)
              (change-class entry 'archive::directory-tar-entry))
            (with-open-file (stream file
                                    :direction :input
                                    :element-type '(unsigned-byte 8)
                                    :if-does-not-exist :error)
              (archive:write-entry-to-archive archive entry :stream stream))))))
    (archive:finalize-archive archive)
    output))

(defun gz (file output &key (if-exists :error))
  (salza2:gzip-file file output :if-exists if-exists))

(defun tgz (files output &rest tar-args &key (if-exists :error) &allow-other-keys)
  (let ((tar (make-pathname :type "tar" :defaults output)))
    (unwind-protect
         (progn (apply #'tar files tar :if-exists :supersede tar-args)
                (gz tar output :if-exists if-exists))
      (delete-file tar))))

(defun enlist (list &rest els)
  (if (listp list) list (list* list els)))

(defun digest (file/s digest)
  (let ((digest (ironclad:make-digest digest)))
    (dolist (file (enlist file/s) (ironclad:byte-array-to-hex-string
                                   (ironclad:produce-digest digest)))
      (with-open-file (stream file :element-type '(unsigned-byte 8) :if-does-not-exist NIL)
        (when stream
          (ironclad:update-digest digest stream))))))

(defun file-size (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun tempfile ()
  (make-pathname :name (format NIL "~36r-~36r" (get-universal-time) (random #xFFFFFFFFFFFFFFFF))
                 :type "dat"
                 :defaults (uiop:temporary-directory)))

(defun file-match-p (file pattern)
  (let ((file (pathname-utils:unix-namestring file))
        (string (pathname-utils:unix-namestring pattern)))
    (if (pathname-utils:absolute-p pattern)
        (starts-with string file :start1 1)
        (loop for i = -1 then (position #\/ file :start i)
              while i
              thereis (progn (incf i) (starts-with string file :start2 i))))))

(defun empty-directory-p (dir)
  (null (filesystem-utils:list-contents dir)))

(defun gather-sources (base &optional exclude)
  (let ((base (truename base)))
    (loop for file in (directory (merge-pathnames (make-pathname :name :wild :type :wild :directory '(:relative :wild-inferiors)) base))
          for relative = (pathname-utils:enough-pathname file base)
          when (and (filesystem-utils:file-p file)
                    (not (loop for excluded in exclude
                               thereis (file-match-p relative excluded))))
          collect file)))

(defun ensure-instance (instance type &rest initargs)
  (cond ((null instance)
         (apply #'make-instance type initargs))
        ((eql type (type-of instance))
         (apply #'reinitialize-instance instance initargs))
        (T
         (apply #'change-class instance type initargs))))

(defun absolutize (path base)
  (multiple-value-bind (absolute-p path) (pathname-utils:absolute-p path)
    (if absolute-p
        path
        (merge-pathnames path base))))

(defun ends-with (end string)
  (and (<= (length end) (length string))
       (string= end string :start2 (- (length string) (length end)))))

(defun starts-with (start string &key (start1 0) (start2 0))
  (and (<= (- (length start) start1) (- (length string) start2))
       (string= start string :start1 start1 :start2 start2 :end2 (+ start2 (- (length start) start1)))))

(defun url-encode (thing &key (stream NIL) (external-format :utf-8) (allowed "-._~"))
  (flet ((%url-encode (stream)
           (loop for octet across (babel:string-to-octets thing :encoding external-format)
                 for char = (code-char octet)
                 do (cond ((or (char<= #\0 char #\9)
                               (char<= #\a char #\z)
                               (char<= #\A char #\Z)
                               (find char allowed :test #'char=))
                           (write-char char stream))
                          (T (format stream "%~2,'0x" (char-code char)))))))
    (if stream
        (%url-encode stream)
        (with-output-to-string (stream)
          (%url-encode stream)))))

(defun coerce-args (args)
  (loop for arg in args
        append (etypecase arg
                 (cons (coerce-args arg))
                 (null ())
                 (pathname (list (pathname-utils:native-namestring arg)))
                 (string (list arg))
                 (number (list (prin1-to-string arg)))
                 (symbol (list (string arg))))))

(defun run (command &rest args)
  (let ((output (make-string-output-stream)))
    (loop (with-simple-restart (retry "Retry running the program")
            (let ((exit (simple-inferiors:run #-windows command #+windows (format NIL "~a.exe" command)
                                              (coerce-args args) :output output :error output)))
              (if (= 0 exit)
                  (return exit)
                  (error "Failed to run~%~%  ~a~{ ~a~}~%~%It exited with code ~a and output:~%~%  ~a"
                         command args exit (get-output-stream-string output))))))))

(defun run-string (command &rest args)
  (loop (with-simple-restart (retry "Retry running the program")
          (return (string-right-trim '(#\Return #\Linefeed #\Space)
                                     (simple-inferiors:run #-windows command #+windows (format NIL "~a.exe" command)
                                                           (coerce-args args) :on-non-zero-exit :error :output :string))))))

(defun fetch (url &optional processor verbose)
  (when verbose (verbose "Fetching ~a" url))
  (let ((data (run-string "curl" "-L" url)))
    (if processor
        (with-input-from-string (stream data)
          (funcall processor stream))
        data)))

(defun prune-plist (plist)
  (loop for (k v) on plist by #'cddr
        when v collect k
        when v collect v))

(defun verbose (format &rest args)
  (let ((str (format NIL "; ~?" format args)))
    (format *error-output* "~&~a~%" str)))

(defun url-extract-name (url)
  ;; FIXME: this sucks
  (let ((slash (position #\/ url :from-end T)))
    (if (and (< (length ".git") (length url))
             (string= ".git" url :start2 (- (length url) (length ".git"))))
        (subseq url (1+ slash) (- (length url) (length ".git")))
        (subseq url (1+ slash)))))

(defun parse-time (time &key time-zone error default)
  (flet ((p (a)
           (if (and a (string/= "" a))
               (parse-integer a)
               0)))
    (or (cl-ppcre:register-groups-bind (yy mm dd h m s) ("(\\d{1,4})[-,./](\\d{1,2})[-,./](\\d{1,2})(?:[tT/ -](\\d{1,2})[:.-](\\d{1,2})(?:[:.-](\\d{1,3}))?)?" time)
          (encode-universal-time (p s) (p m) (p h) (p dd) (p mm) (p yy) time-zone))
        (cl-ppcre:register-groups-bind (h m s) ("[tT]?(\\d{1,2})[:.-](\\d{1,2})(?:[:.-](\\d{1,3}))?" time)
          (+ (p s) (* (p m) 60) (* (p h) 60 60)))
        (if error
            (error "Cannot parse ~s into a time." time)
            default))))

(defmacro do-list* ((el list) &body body)
  (let ((l (gensym "LIST"))
        (thunk (gensym "THUNK")))
    `(let ((,l ,list))
       (flet ((,thunk (,el)
                ,@body))
         (if lparallel:*kernel*
             (lparallel:pmapcar #',thunk ,l)
             (mapcar #',thunk ,l))))))

(defmacro with-kernel (jobs &body body)
  (let ((j (gensym "JOBS"))
        (thunk (gensym "THUNK")))
    `(let ((,j ,jobs))
       (flet ((,thunk ()
                ,@body))
         (if ,j
             (let ((lparallel:*kernel* (lparallel:make-kernel ,j)))
               (unwind-protect (,thunk)
                 (lparallel:end-kernel)))
             (,thunk))))))

(defun relpath (path parent)
  (namestring (pathname-utils:enough-pathname path parent)))

(defun arg! (initarg)
  (error "~s required." initarg))

(defgeneric version< (a b)
  (:method ((a real) (b real)) (< a b))
  (:method ((a string) (b string)) (string< a b)))

(defun version> (a b)
  (version< b a))

(defgeneric serialize (thing)
  (:method-combination append :most-specific-last))

(defun hash (obj)
  (let ((state (ironclad:make-digest :sha256)))
    (labels ((rec (obj)
               (etypecase obj
                 (string (rec (babel:string-to-octets obj)))
                 (vector (ironclad:update-digest state obj))
                 (standard-object (rec (version-hash obj)))
                 (list (mapc #'rec obj)))))
      (rec obj))
    (ironclad:byte-array-to-hex-string
     (ironclad:produce-digest state))))

(defun format-timestamp (&optional (format :datetime) (stamp (get-universal-time)))
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time stamp 0)
    (ecase format
      (:datetime
       (format NIL "~4,'0d.~2,'0d.~2,'0d-~2,'0d.~2,'0d.~2,'0d" yy mm dd h m s))
      (:date
       (format NIL "~4,'0d-~2,'0d-~2,'0d" yy mm dd))
      (:time
       (format NIL "~2,'0d.~2,'0d.~2,'0d" h m s)))))
