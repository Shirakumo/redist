#|
 This file is a part of Shirakumo-Dist
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:shirakumo-dist
  (:use #:cl)
  (:nicknames #:org.shirakumo.dist)
  ;; dist.lisp
  (:export
   #:*server-port*
   #:*dist-name*
   #:*dist-url*
   #:*releases-dir*
   
   #:redist
   #:purge-all-releases
   
   #:start-dist-server
   #:stop-dist-server)
  ;; git.lisp
  (:export
   #:git
   #:git-clone
   #:git-pull
   #:git-reset
   #:git-submodule-update
   #:git-rev-parse
   #:git-repository-p)
  ;; sources.lisp
  (:export
   #:*repositories*
   #:*repositories-file*
   #:*sources-dir*
   
   #:load-repositories
   #:dump-repositories
   
   #:add-repository
   #:remove-repository
   #:have-repository
   
   #:repository-name
   #:repository-directory
   #:repository-exists-p
   
   #:create-repository
   #:redraw-repository
   #:ensure-repository-ready
   
   #:purge-unknown-repositories
   #:purge-all-sources
   #:ensure-repositories-ready
   #:redraw)
  ;; toolkit.lisp
  (:export
   #:timestamp
   #:search*))
