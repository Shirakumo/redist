#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.redist
  (:shadow #:compile)
  (:use #:cl)
  ;; toolkit.lisp
  (:export)
  ;; asdf.lisp
  (:export
   #:find-file-systems
   #:find-asd-files
   #:find-all-systems)
  ;; dist.lisp
  (:export
   #:*excluded-paths*
   #:make-release
   #:find-project
   #:find-release
   #:next-version
   #:version<
   #:dist
   #:name
   #:url
   #:projects
   #:releases
   #:excluded-paths
   #:integer-versioned-dist
   #:timestamp-versioned-dist
   #:source-manager
   #:url
   #:version
   #:update
   #:clone
   #:project
   #:name
   #:source-directory
   #:sources
   #:active-p
   #:excluded-systems
   #:excluded-paths
   #:remove-project
   #:add-project
   #:release
   #:dist
   #:version
   #:projects
   #:releases-url
   #:systems-url
   #:releases-path
   #:systems-path
   #:project-release
   #:dist
   #:project
   #:release
   #:version
   #:systems
   #:source-files
   #:path
   #:prefix
   #:system
   #:project
   #:name
   #:file
   #:dependencies)
  ;; persistence.lisp
  (:export
   #:dist
   #:serialize
   #:persist
   #:restore
   #:define-dist)
  ;; sources.lisp
  (:export
   #:cvs
   #:svn
   #:darcs
   #:mercurial
   #:git
   #:http
   #:github
   #:gitlab)
  ;; ql-support.lisp
  (:export
   #:parse-quicklisp-source
   #:parse-quicklisp-projects
   #:parse-quicklisp-source-file)
  ;; compile.lisp
  (:export
   #:compile))
