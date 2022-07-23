#|
 This file is a part of Shirakumo-Dist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defmethod eclector.reader:interpret-symbol ((client (eql 'reader)) in package symbol intern)
  (handler-case (call-next-method)
    ((or
      eclector.reader:package-does-not-exist
      eclector.reader:symbol-does-not-exist
      eclector.reader:symbol-is-not-external) ()
      (intern symbol "KEYWORD"))))

(defmethod eclector.reader:find-character ((client (eql 'reader)) name)
  (or (name-char name) (code-char #xFFFD)))

(defmethod eclector.reader:evaluate-expression ((client (eql 'reader)) expr)
  NIL)

(defmethod walk ((form cons) acc)
  ;; We do some simple walking here. If you do anything more crazy than that, good luck.
  (case (first form)
    ((asdf:defsystem)
     (push (cons (second form) (find-defsystem-dependencies form)) (cdr acc)))
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
     (walk (car form) acc)
     (walk (cdr form) acc))))

(defmethod walk (form acc))

(defun resolve-dependency (dep)
  ;; We use ASDF itself as a dummy system here -- we don't actually care about what happens
  ;; if it's not a foreign dependency, and if you customise how this resolution happens...
  ;; good luck to ya!
  (typecase dep
    (null)
    (asdf:system (string (asdf:component-name dep)))
    (string dep)
    (symbol (string-downcase dep))
    (cons
     (resolve-dependency
      (case (first dep)
        (:feature (third dep))
        (:version (second dep))
        (:require NIL)
        (T (ignore-errors (asdf/defsystem::resolve-dependency-spec (asdf:find-system "asdf") dep))))))))

(defun find-defsystem-dependencies (form)
  (let ((deps ()))
    (loop for (k v) on form by #'cddr
          do (when (and (find k '(:depends-on :defsystem-depends-on))
                        (typep v 'list))
               (loop for depspec in v
                     for dep = (resolve-dependency depspec)
                     do (when dep
                          (push dep deps)))))
    deps))

(defun find-file-systems (file)
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
        append (loop for (system . deps) in (find-file-systems file)
                     collect (list (string system) file deps))))
