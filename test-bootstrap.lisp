#|
 This file is a part of Shirakumo-Dist
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(format T ";;; Starting bootstrap.")

(defpackage #:!boot
  (:nicknames #:org.shirakumo.dist.test.boot)
  (:use #:cl)
  (:export
   #:load-manifest
   #:test-system
   #:test!))
(in-package #:org.shirakumo.dist.test.boot)

(require 'asdf)

;;;;;;
;; ASDF SETUP
(defvar *system-table* (make-hash-table :test 'equalp)
  "EQUALP hash table mapping system names to ASDF files.")
(defvar *log-file* (merge-pathnames "bootstrap-test.log" (or (uiop:getcwd) *load-pathname* #p"~/"))
  "Target log file to which all output is directed.")

(defun search-for-system (name)
  (gethash name *system-table*))

(defun read-manifest-file (manifest)
  (with-open-file (stream manifest :direction :input :if-does-not-exist :error)
    (loop for line = (read-line stream NIL NIL)
          while line
          for pathname = (uiop:parse-native-namestring line)
          when (uiop:file-exists-p pathname)
          collect pathname)))

(defun load-manifest (manifest)
  (format T "~&; Loading manifest from ~a~%" manifest)
  (dolist (pathname (read-manifest-file manifest))
    (setf (gethash (pathname-name pathname) *system-table*)
          (probe-file pathname))))

(defmacro with-timing ((thing &rest format-args) &body body)
  (let ((time (gensym "TIME"))
        (thing `(format NIL ,thing ,@format-args)))
    `(let ((,time (get-internal-real-time)))
       (format T "~&; Begin ~a~%" ,thing)
       ,@body
       (format T "~&; End ~a (~fs)~%" ,thing
               (/ (- (get-internal-real-time) ,time) internal-time-units-per-second)))))

(defmethod asdf:perform :around ((op asdf:compile-op) (system asdf:system))
  (with-timing ("COMPILE ~a" (asdf:component-name system))
    (call-next-method)))

(defmethod asdf:perform :around ((op asdf:load-op) (system asdf:system))
  (with-timing ("LOAD ~a" (asdf:component-name system))
    (call-next-method)))

(defun setup-externals ()
  (push #'search-for-system asdf:*system-definition-search-functions*))

;;;;;;
;; LOGGING AND REPORTING
(defun format-universal-time (&optional (ut (get-universal-time)))
  (format NIL "~:@{~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d~}"
          (subseq (nreverse (multiple-value-list (decode-universal-time ut))) 3)))

(defmacro with-logging ((&optional (file '*log-file*)) &body body)
  (let ((stream (gensym "STREAM")))
    `(with-open-file (,stream ,file :direction :output :if-exists :supersede :if-does-not-exist :create)
       (let ((,stream (make-broadcast-stream ,stream *standard-output*)))
         (unwind-protect
              (let ((*standard-output* ,stream)
                    (*error-output* ,stream)
                    (*debug-io* ,stream))
                (format T "
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~a~%~%~%" (format-universal-time))
                (unwind-protect
                     (progn ,@body)
                  (format T "~%
;; ~a
;;;;;;;;;;;;;;;;;;;;;;;;;;" (format-universal-time))))
           (close ,stream))))))

#+:sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol "*DEBUG-PRINT-VARIABLE-ALIST*" :sb-debug)
    (pushnew :sbcl-debug-print-variable-alist *features*)))

#+:sbcl
(defun print-backtrace ()
  (let (#+:sbcl-debug-print-variable-alist
	(sb-debug:*debug-print-variable-alist*
          (list* '(*print-level* . nil)
                 '(*print-length* . nil)
                 sb-debug:*debug-print-variable-alist*))
	#-:sbcl-debug-print-variable-alist
	(sb-debug:*debug-print-level* nil)
	#-:sbcl-debug-print-variable-alist
	(sb-debug:*debug-print-length* nil))
    (sb-debug:backtrace)))

#+:ccl
(defun print-backtrace ()
  (ccl:print-call-history :detailed-p nil))

#-(or :sbcl :ccl)
(defun print-backtrace ()
  (format T "Don't know how to print the backtrace on ~a, sorry!"
          (lisp-implementation-type)))

(defun error-trap (error)
  (format T "~&

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~a

Failure Occurred:
-----------------

Error of type [~s]
   ~a

Backtrace:~%~%" (format-universal-time) (type-of error) error)
  (print-backtrace))

(defmacro with-error-trap (() &body body)
  `(restart-case
       (handler-bind ((error #'(lambda (err)
                                 (error-trap err)
                                 (invoke-restart 'exit))))
         ,@body)
     (exit ()
       :report "Exit the implementation."
       #+:sbcl (sb-ext:exit :code 1)
       #+:ccl (ccl:quit 1)
       #+:abcl (ext:quit :status 1)
       #+:ecl (ext:quit 1))))

;;;;;;
;; ACTUAL TESTING
(declaim (inline test-system))
(defun test-system (system-name)
  (format T "~&;; Testing System ~a~%" system-name)
  (asdf:compile-system system-name :verbose T)
  (asdf:load-system system-name :verbose T)
  (format T "~&;; System Test Successful~%"))

(defun kill-self (&optional (package #.*package*))
  (format T "~&;; Killing self.~%")
  (do-symbols (symbol package)
    (when (eql (symbol-package symbol) package)
      (when (fboundp symbol)
        (fmakunbound symbol))
      (when (boundp symbol)
        (makunbound symbol))
      (unintern symbol package)))
  (delete-package package)
  #+:sbcl (sb-ext:gc :full T))

(defun test! (system-name &key manifest (log-file *log-file*))
  (with-logging (log-file)
    (with-error-trap ()
      (when manifest
        (load-manifest manifest))
      (setup-externals)
      (test-system system-name))))
