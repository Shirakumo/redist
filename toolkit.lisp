#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.redist)

(defvar *here* #.(make-pathname :name NIL :type NIL :defaults (or *compile-file-pathname* *load-pathname*)))

(defun tar (files output &key (if-exists :error) (base #p"/") (archive-root #p""))
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
    (apply #'tar files tar :if-exists :error tar-args)
    (unwind-protect
         (gz tar output :if-exists if-exists)
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
  (let ((file (uiop:unix-namestring file))
        (string (uiop:unix-namestring pattern)))
    (if (pathname-utils:absolute-p pattern)
        (starts-with string file :start1 1)
        (loop for i = -1 then (position #\/ file :start i)
              while i
              thereis (progn (incf i) (starts-with string file :start2 i))))))

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

(defun split (split string)
  (let ((parts ()) (buffer (make-string-output-stream)))
    (flet ((maybe-output ()
             (let ((part (get-output-stream-string buffer)))
               (when (string/= part "") (push part parts)))))
      (loop for char across string
            do (if (char= char split)
                   (maybe-output)
                   (write-char char buffer))
            finally (maybe-output))
      (nreverse parts))))

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
        collect (etypecase arg
                  (pathname (pathname-utils:native-namestring arg))
                  (string arg)
                  (number (prin1-to-string arg))
                  (symbol (string arg)))))

(defun run (command &rest args)
  (loop (with-simple-restart (retry "Retry running the program")
          (return (simple-inferiors:run #-windows command #+windows (format NIL "~a.exe" command)
                                        (coerce-args args) :on-non-zero-exit :error)))))

(defun run-string (command &rest args)
  (loop (with-simple-restart (retry "Retry running the program")
          (return (string-right-trim '(#\Return #\Linefeed #\Space)
                                     (simple-inferiors:run #-windows command #+windows (format NIL "~a.exe" command)
                                                           (coerce-args args) :on-non-zero-exit :error :output :string))))))

(defun prune-plist (plist)
  (loop for (k v) on plist by #'cddr
        when v collect k
        when v collect v))

(defun verbose (format &rest args)
  (format *error-output* "~&; ~?~%" format args))

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
    (or (cl-ppcre:register-groups-bind (yy mm dd h m s) ("(\\d{1,4})[-,./](\\d{1,2})[-,./](\\d{1,2})(?:[tT/ ](\\d{1,2})[:-](\\d{1,2})(?:[:-](\\d{1,3}))?)?" time)
          (encode-universal-time (p s) (p m) (p h) (p dd) (p mm) (p yy) time-zone))
        (cl-ppcre:register-groups-bind (h m s) ("[tT]?(\\d{1,2})[:-](\\d{1,2})(?:[:-](\\d{1,3}))?" time)
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
