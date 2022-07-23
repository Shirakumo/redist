#|
 This file is a part of Shirakumo-Dist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

;;; toolkit.lisp
(docs:define-docs)

;;; asdf.lisp
(docs:define-docs
  (function find-file-systems
    "Find all system definitions within the given file.

Returns a list of the following structure:
  ((SYSTEM-NAME SYSTEM-DEPENDENCY*)*)

See FIND-ALL-SYSTEMS")
  
  (function find-asd-files
    "Returns all ASD files in the given directory, recursively.

See FIND-ALL-SYSTEMS")
  
  (function find-all-systems
    "Returns all system definitions within the given directory, recursively.

See FIND-ASD-FILES
See FIND-FILE-SYSTEMS"))

;;; dist.lisp
(docs:define-docs
  (variable *excluded-paths*
    "")
  
  (function make-release
    "")
  
  (function find-project
    "")
  
  (function find-release
    "")
  
  (function next-version
    "")
  
  (function version<
    "")
  
  (type dist
    "")
  
  (function name
    "")
  
  (function url
    "")
  
  (function projects
    "")
  
  (function releases
    "")
  
  (function excluded-paths
    "")
  
  (function integer-versioned-dist
    "")
  
  (function timestamp-versioned-dist
    "")
  
  (type source-manager
    "")
  
  (function version
    "")
  
  (function update
    "")
  
  (function clone
    "")
  
  (type project
    "")
  
  (function source-directory
    "")
  
  (function sources
    "")
  
  (function active-p
    "")
  
  (function excluded-systems
    "")
  
  (function remove-project
    "")
  
  (function add-project
    "")
  
  (type release
    "")
  
  (function dist
    "")
  
  (function releases-url
    "")
  
  (function systems-url
    "")
  
  (function releases-path
    "")
  
  (function systems-path
    "")
  
  (type project-release
    "")
  
  (function project
    "")
  
  (function release
    "")
  
  (function systems
    "")
  
  (function source-files
    "")
  
  (function path
    "")
  
  (function prefix
    "")
  
  (type system
    "")
  
  (function file
    "")
  
  (function dependencies
    ""))

;;; persistence.lisp
(docs:define-docs
  (function serialize
    "")
  
  (function persist
    "")
  
  (function restore
    "")
  
  (function define-dist
    ""))

;;; sources.lisp
(docs:define-docs
  (type cvs
    "")
  
  (type svn
    "")
  
  (type darcs
    "")
  
  (type mercurial
    "")
  
  (type git
    "")
  
  (type http
    "")
  
  (type github
    "")
  
  (type gitlab
    ""))

;;; ql-support.lisp
(docs:define-docs
  (function parse-quicklisp-source
    "")
  
  (function parse-quicklisp-projects
    "")
  
  (function parse-quicklisp-source-file
    ""))

;;; compile.lisp
(docs:define-docs
  (function compile
    ""))
