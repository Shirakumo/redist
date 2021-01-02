#|
 This file is a part of Shirakumo-Dist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defstruct placeholder name package)

(defmethod eclector.reader:interpret-symbol ((client (eql 'reader)) in package symbol intern)
  (handler-case (call-next-method)
    ((or
      eclector.reader:package-does-not-exist
      eclector.reader:symbol-does-not-exist
      eclector.reader:symbol-is-not-external) ()
      (make-placeholder :package package :name symbol))))

(defmethod walk :around (form acc)
  (handler-case (call-next-method)
    (error (e)
      (format T "~& Failed to walk form:~%  ~a~%Failure: ~a" form e))))

(defmethod walk ((form cons) acc)
  ;; We do some simple walking here. If you do anything more crazy than that, good luck.
  (case (first form)
    ((asdf:defsystem)
     (push (second form) (cdr acc)))
    ((let)
     (dolist (bind (second form))
       (when (listp bind)
         (dolist (part (rest bind))
           (walk part acc))))
     (dolist (part (cddr form))
       (walk part acc)))
    ((flet labels macrolet symbol-macrolet)
     (dolist (part (cddr form))
       (walk part acc)))
    ((defmacro defun defvar defparameter defgeneric defmethod defclass defstruct))
    (T
     (dolist (part (cdr form))
       (walk part acc)))))

(defmethod walk (form acc))

(defun find-systems (file)
  (with-open-file (stream file)
    (let ((*package* (find-package :asdf/user))
          (eclector.reader:*client* 'reader)
          (acc (cons NIL NIL)))
      (loop for form = (eclector.reader:read stream NIL #1='#:eof)
            until (eq form #1#)
            do (walk form acc))
      (cdr acc))))

(defun find-asd-files (root)
  (directory (merge-pathnames (make-pathname :name :wild :type "asd" :directory '(:relative :wild-inferiors))
                             root)))

(defun find-all-systems (root)
  (loop for file in (find-asd-files root)
        append (loop for system in (find-systems file)
                     collect (list file (string system)))))
