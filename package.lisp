(defpackage #:org.shirakumo.redist
  (:shadow #:compile)
  (:use #:cl)
  (:local-nicknames
   (#:filesystem-utils #:org.shirakumo.filesystem-utils)
   (#:pathname-utils #:org.shirakumo.pathname-utils))
  ;; toolkit.lisp
  (:export
   #:version<
   #:serialize)
  ;; asdf.lisp
  (:export
   #:find-file-systems
   #:find-asd-files
   #:find-all-systems)
  ;; dist.lisp
  (:export
   #:make-release
   #:find-project
   #:find-release
   #:next-version
   #:dist
   #:name
   #:url
   #:projects
   #:releases
   #:excluded-paths
   #:integer-versioned-dist
   #:timestamp-versioned-dist
   #:date-versioned-dist
   #:define-dist)
  ;; plaintext.lisp
  (:export
   #:plaintext
   #:dir
   #:id-counter)
  ;; project.lisp
  (:export
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
   #:define-project
   #:quick-add-projects)
  ;; release.lisp
  (:export
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
  ;; replicate.lisp
  (:export
   #:replicate-dist
   #:replicate-dist-version)
  ;; sources.lisp
  (:export
   #:source-manager
   #:url
   #:version
   #:update
   #:clone
   #:cvs
   #:svn
   #:darcs
   #:mercurial
   #:git
   #:http
   #:github
   #:gitlab
   #:dist-source)
  ;; storage.lisp
  (:export
   #:*storage*
   #:*storage-file*
   #:storage-file
   #:storage
   #:file
   #:open-storage
   #:retrieve
   #:store
   #:list-storage-file-types
   #:try-open-storage
   #:stored-object
   #:id
   #:stored-p
   #:retrieve-all)
  ;; ql-support.lisp
  (:export
   #:parse-quicklisp-source
   #:parse-quicklisp-projects
   #:parse-quicklisp-source-file)
  ;; compile.lisp
  (:export
   #:*default-output-directory*
   #:compile))
