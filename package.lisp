(defpackage #:org.shirakumo.redist
  (:shadow #:compile)
  (:use #:cl)
  (:local-nicknames
   (#:filesystem-utils #:org.shirakumo.filesystem-utils)
   (#:pathname-utils #:org.shirakumo.pathname-utils))
  ;; toolkit.lisp
  (:export)
  ;; asdf.lisp
  (:export
   #:find-file-systems
   #:find-asd-files
   #:find-all-systems)
  ;; dist.lisp
  (:export
   #:*default-source-directory*
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
   #:date-versioned-dist
   #:source-manager
   #:url
   #:version
   #:update
   #:clone
   #:project
   #:name
   #:source-directory
   #:sources
   #:disabled-p
   #:excluded-systems
   #:excluded-paths
   #:releases
   #:remove-project
   #:add-project
   #:release
   #:dist
   #:version
   #:timestamp
   #:projects
   #:releases-url
   #:systems-url
   #:dist-url
   #:releases-path
   #:systems-path
   #:dist-path
   #:project-release
   #:project
   #:version
   #:systems
   #:source-files
   #:archive-md5
   #:source-sha1
   #:path
   #:prefix
   #:system
   #:project
   #:name
   #:file
   #:dependencies)
  ;; persistence.lisp
  (:export
   #:*distinfo-file*
   #:dist
   #:project
   #:serialize
   #:persist
   #:restore
   #:define-project
   #:define-dist
   #:main)
  ;; replicate.lisp
  (:export
   #:replicate-dist
   #:replicate-dist-version)
  ;; sources.lisp
  (:export
   #:cvs
   #:svn
   #:darcs
   #:mercurial
   #:git
   #:http
   #:github
   #:gitlab
   #:dist-source)
  ;; ql-support.lisp
  (:export
   #:parse-quicklisp-source
   #:parse-quicklisp-projects
   #:parse-quicklisp-source-file)
  ;; compile.lisp
  (:export
   #:*default-output-directory*
   #:compile))
