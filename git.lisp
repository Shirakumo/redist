#|
 This file is a part of Shirakumo-Dist
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defun git (commands &key directory (output T) (error T))
  "Performs arbitrary GIT commands. See UIOP:RUN-PROGRAM.
COMMANDS should be a list of command strings. Sub-lists are spliced into the list for convenience."
  (when directory (uiop:chdir directory))
  (uiop:run-program (list* "git" (splice-sublists commands)) :output output :error-output output :ignore-error-status (not error)))

(defun git-clone (directory repository &key branch depth submodules
                                            (error T) (output T))
  (let ((name (or (pathname-name directory)
                  (car (last (pathname-directory directory)))))
        (directory (uiop:pathname-parent-directory-pathname directory)))
    (git (list "clone"
               (when branch (list "-b" branch))
               (when depth (list "--depth" (write-to-string depth)))
               (when submodules "--recurse-submodules")
               repository
               name)
         :directory directory :error error :output output)))

(defun git-pull (directory &key repository refspec submodules all depth
                                (error T) (output T))
  (format T "~&Pulling ~a... " (car (last (pathname-directory directory))))
  (git (list "pull"
             (when submodules "--recurse-submodules")
             (when all "--all")
             (when depth (list "--depth" (write-to-string depth)))
             repository
             refspec)
       :directory directory :error error :output output))

(defun git-reset (directory &key (mode :mixed) commit
                                 (error T) (output T))
  (git (list "reset"
             (ecase mode
               (:soft "--soft")
               (:mixed "--mixed")
               (:hard "--hard")
               (:merge "--merge")
               (:keep "--keep"))
             commit)
       :directory directory :error error :output output))

(defun git-submodule-update (directory &key path init force remote no-fetch checkout
                                            merge rebase reference recursive depth
                                            (error T) (output T))
  (git (list "submodule" "update"
             (when init "--init")
             (when force "--force")
             (when remote "--remote")
             (when no-fetch "--no-fetch")
             (when checkout "--checkout")
             (when merge "--merge")
             (when rebase "--rebase")
             (when reference (list "--reference" reference))
             (when recursive "--recursive")
             (when depth (list "--depth" (write-to-string depth)))
             path)
       :directory directory :error error :output output))

(defun git-rev-parse (directory &key revs-only no-revs flags no-flags
                                     default short symbolic symbolic-full-name
                                     all branches tags remotes glob exclude
                                     disambiguate local-env-vars git-dir
                                     is-inside-git-dir is-inside-work-tree
                                     is-bare-repository resolve-git-dir
                                     show-cdump show-prefix show-toplevel
                                     shared-index-path since after until before
                                     (error T) (output T))
  (git (list "rev-parse"
             (when revs-only "--revs-only")
             (when no-revs "--no-revs")
             (when flags "--flags")
             (when no-flags "--no-flags")
             (when default (list "--default" default))
             (when short (list* "--short" (when (integerp short) (list short))))
             (when symbolic "--symbolic")
             (when symbolic-full-name "--symbolic-full-name")
             (when all "--all")
             (when branches (format NIL "--branches~:[=~a~;~]" (eql branches T) branches))
             (when tags (format NIL "--tags~:[=~a~;~]" (eql tags T) tags))
             (when remotes (format NIL "--remotes~:[=~a~;~]" (eql remotes T) remotes))
             (when glob (format NIL "--glob=~a" glob))
             (when exclude (format NIL "--exclude=~a" exclude))
             (when disambiguate (format NIL "--disambiguate=~a" disambiguate))
             (when local-env-vars "--local-env-vars")
             (when git-dir "--git-dir")
             (when is-inside-git-dir "--is-inside-git-dir")
             (when is-inside-work-tree "--is-inside-work-tree")
             (when is-bare-repository "--is-bare-repository")
             (when resolve-git-dir (list "--resolve-git-dir" resolve-git-dir))
             (when show-cdump "--show-cdump")
             (when show-prefix "--show-prefix")
             (when show-toplevel "--show-toplevel")
             (when shared-index-path "--shared-index-path")
             (when since (format NIL "--since=~a" since))
             (when after (format NIL "--after=~a" after))
             (when until (format NIL "--until=~a" until))
             (when before (format NIL "--before=~a" before)))
       :directory directory :error error :output output))

(defun git-repository-p (directory)
  (when (uiop:directory-exists-p directory)
    (not (string= (git-rev-parse directory :git-dir T :error NIL :output :string) ""))))
