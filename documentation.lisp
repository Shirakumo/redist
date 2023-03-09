#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.redist)

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
  (variable *default-source-directory*
    "The default directory under which source repositories are stored.

See PROJECT")
  
  (variable *excluded-paths*
    "A list of path patterns to exclude globally.

This list is used for *all* dists and projects.

See EXCLUDED-PATHS")
  
  (function make-release
    "Create a new release for the given object.

You may optionally pass a VERSION to target, whether to UPDATE the
projects involved, and whether to be VERBOSE about the goings-on. For
a DIST release, you may also explicitly pass the list of PROJECTS to
be included in the release.

If UPDATE is passed, the involved PROJECTs are first UPDATEd.

When making a release of an individual project, you must pass the DIST
that a release is being made for, as well as the RELEASE object
associated with the individual release.

See UPDATE
See DIST (type)
See PROJECT (type)
See RELEASE (type)
See PROJECT-RELEASE (type)")
  
  (function find-project
    "Returns the object identified by the given name on the DIST or RELEASE, if any.

If no such object exists, NIL is returned.

See DIST (type)
See RELEASE (type)
See PROJECT (type)")
  
  (function find-release
    "Returns the release identified by the given version on the DIST.

If no such release exists, NIL is returned.

See DIST (type)
See RELEASE (type)")
  
  (function next-version
    "Returns the next version for the dist that should be used as a release version identifier.

Note that each invocation of this function must return an object that
is not VERSION< to the object returned by the previous invocation.

See DIST (type)
See VERSION<")
  
  (function version<
    "Returns T if A is a version preceding B.

A user may extend this with additional functions, but by default
methods for REALs and STRINGs are provided.")
  
  (type dist
    "Representation of a Quicklisp distribution.

A distribution presents a source for software releases. As such, a
dist encompasses a number of PROJECTs and a number of RELEASEs of a
subset of those projects.

You must not create instances of the base DIST type. You should
instead instantiate a subclass, which implements the NEXT-VERSION
method, such as INTEGER-VERSIONED-DIST or TIMESTAMP-VERSIONED-DIST.

See NAME
See URL
See PROJECTS
See RELEASES
See EXCLUDED-PATHS
See MAKE-RELEASE
See FIND-PROJECT
See FIND-RELEASE
See NEXT-VERSION
See COMPILE
See INTEGER-VERSIONED-DIST (type)
See TIMESTAMP-VERSIONED-DIST (type)")
  
  (function name
    "Accesses the name of the object.

See DIST (type)
See PROJECT (type)
See PROJECT-RELEASE (type)
See SYSTEM (type)")
  
  (function url
    "Accesses the URL at which the object is accessible in the context of a dist release.

See DIST (type)
See PROJECT-RELEASE (type)
See SOURCE-MANAGER (type)")
  
  (function projects
    "Accesses the set of projects associated with the object.

In the case of a DIST, the set of projects may only increase, not
decrease. If you attempt to remove projects, they will instead be
deactivated. You may however add new projects this way. Any argument
accepted by ADD-PROJECT will also work here and be appropriately
coerced.

In the case of a RELEASE, the set of instances is of PROJECT-RELEASEs
instead. These PROJECT-RELEASEs may however belong to a different
release, if the project's sources have not materially changed between
the releases. You may add new project-releases onto this set by either
including a PROJECT, a PROJECT-RELEASE, or a list of the following
spec:

  (PROJECT &rest INITARGS &key SYSTEMS)
  SYSTEMS ::= ((NAME . INITARGS)*)

See DIST (type)
See RELEASE (type)
See ADD-PROJECT
See REMOVE-PROJECT")
  
  (function releases
    "Accesses the set of releases present on the DIST or PROJECT.

When setting this place, the set is automatically ordered such that
the latest version comes first. Each object in the set is also coerced
to become a RELEASE instance, meaning that if it is not a RELEASE, it
may either be a VERSION specifying the version at which a new release
should be made, or a list of the following spec:

  (VERSION . INITARGS)

Note that no two releases may have the same (under EQUAL) version.

See MAKE-RELEASE
See RELEASE (type)
See PROJECT-RELEASE (type)
See DIST (type)
See PROJECT (type)")
  
  (function excluded-paths
    "Accesses the set of path exclusion pattern.

Each item in the list should be a path, describing a pattern as follows:

If the pattern is an absolute path, any path that is a subpath of the
absolute path matches. If the pattern is a relative path, any path
which contains the pattern at any point matches.

Eg:
|| PATTERN  | PATH         | RESULT ||
|| /foo/bar | /foo/bar     | T      ||
|| /foo/bar | /foo/bar/baz | T      ||
|| /foo/bar | /baz/foo/bar | NIL    ||
|| /foo     | /foo/bar     | T      ||
|| foo      | /foo/bar     | T      ||
|| foo      | /baz/foo/bar | T      ||
|| foo/bar  | /baz/foo/bar | T      ||
|| foo      | /baz/bar     | NIL    ||

See DIST (type)
See PROJECT (type)")
  
  (function integer-versioned-dist
    "A dist whose versioning scheme is a simple monotonically increasing integer.

See DIST (type)
See NEXT-VERSION")
  
  (function timestamp-versioned-dist
    "A dist whose versioning scheme is a timestamp at the version creation time.

The timestamp is returned as a string that may be lexicographically
ordered correctly.

See DIST (type)
See NEXT-VERSION")
  
  (type source-manager
    "Representation of a remote source repository.

See URL
See VERSION
See UPDATE
See CLONE")
  
  (function version
    "Returns the version of the given object.

For a SOURCE-MANAGER this is the version of the source code it
manages. The SIMPLE-INFERIORS:*CWD* must be within the local source
code repository clone for this to work properly.

For a RELEASE it is the version of the dist at which the release was
created.

For a PROJECT-RELEASE it is the version at which the PROJECT's
SOURCE-MANAGER was at the time the release was created.

For a PROJECT it returns the current version of the locally cloned
source code.

See SIMPLE-INFERIORS:*CWD*
See SOURCE-MANAGER (type)
See RELEASE (type)
See PROJECT (type)
See PROJECT-RELEASE (type)")
  
  (function update
    "Attempts to update the source code.

You may specify a VERSION which, if possible, the SOURCE-MANAGER will
try to fetch. If no VERSION is specified, the latest version is
picked.

The SIMPLE-INFERIORS:*CWD* must be within the local source
code repository clone for this to work properly.

See SOURCE-MANAGER (type)
See PROJECT (type)
See CLONE")
  
  (function clone
    "Attempts to clone the source code repository.

You may specify a VERSION which, if possible, the SOURCE-MANAGER will
try to fetch. If no VERSION is specified, the latest version is
picked.

The SIMPLE-INFERIORS:*CWD* must be within the directory to which the
clone should be made.

See SOURCE-MANAGER (type)
See PROJECT (type)
See UPDATE")
  
  (type project
    "Representation of a project to be distributed via a dist.

A project encompasses a source from which source code is obtained, and
a number of ASDF systems which are present within the source code.

When a project is re/initialized, it will attempt to clone the project
locally if it does not exist already, or if it is marked as
inactive. If the cloning fails, a restart called DEACTIVATE is
present, which will abort the clone and mark the project as
deactivated instead.

See NAME
See SOURCE-DIRECTORY
See SOURCES
See RELEASES
See DISABLED-P
See EXCLUDED-SYSTEMS
See EXCLUDED-PATHS
See MAKE-RELEASE
See SYSTEMS
See REMOVE-PROJECT
See ADD-PROJECT
See UPDATE
See CLONE
See VERSION")
  
  (function source-directory
    "Accesses the source directory to which the project's source files are cloned.

See PROJECT (type)")
  
  (function sources
    "Accesses the set of SOURCE-MANAGERs which manage the project's sources.

When UPDATEing or CLONEing the project, this list is consulted in
order to try to manage the project. This allows fallback sources to be
instated in case links go stale or the network breaks down.

See PROJECT (type)
See SOURCE-MANAGER (type)")
  
  (function disabled-p
    "Accesses whether the project is considered active or not.

Projects that are disabled are normally excluded from updates and new
releases of a dist.

See PROJECT (type)")
  
  (function excluded-systems
    "Accesses the list of systems which should be excluded from the list of provided systems by the project.

The systems should be provided as string names.

See PROJECT (type)")
  
  (function remove-project
    "Removes the given project from the dist.

Note that this does not remove it entirely, it merely marks the
project as inactive, so that it will effectively be removed from any
future dist releases.

See PROJECT (type)
See DIST (type)")
  
  (function add-project
    "Adds a new project to the dist.

If a project with the same name already exists, an error is signalled.

The project may be specified by a pathname designating its source
directory, or by another spec as described in PROJECTS.

See PROJECTS
See PROJECT (type)
See DIST (type)")
  
  (type release
    "Represents a snapshot of projects within a dist.

A release encompasses a set of projects at a specific version within a
dist, with a unique version naming this set.

See DIST (type)
See PROJECT-RELEASE (type)
See PROJECT (type)
See DIST
See VERSION
See PROJECTS
See FIND-PROJECT
See RELEASES-URL
See SYSTEMS-URL
See DIST-URL
See RELEASES-PATH
See SYSTEMS-PATH
See DIST-PATH
See COMPILE")
  
  (function dist
    "Accesses the dist under which this object was created.

If passed a symbol, the dist with the given name is retrieved (if
any).

See DIST (type)
See RELEASE (type)
See PROJECT-RELEASE (type)")
  
  (function releases-url
    "Returns the URL at which the releases.txt file can be found for the release.

See RELEASE (type)")
  
  (function systems-url
    "Returns the URL at which the systems.txt file can be found for the release.

See RELEASE (type)")

  (function dist-url
    "Returns the URL at which the dist.txt file can be found for the release.

See RELEASE (type)")
  
  (function releases-path
    "Returns the local path at which the releases.txt file can be found for the release.

See RELEASE (type)")
  
  (function systems-path
    "Returns the local path at which the systems.txt file can be found for the release.

See RELEASE (type)")

  (function dist-path
    "Returns the local path at which the dist.txt file can be found for the release.

See RELEASE (type)")
  
  (type project-release
    "Represents a particular version snapshot of a project.

See PROJECT (type)
See PROJECT
See VERSION
See SYSTEMS
See SOURCE-FILES
See NAME
See URL
See ARCHIVE-MD5
See SOURCE-SHA1
See PATH
See PREFIX")
  
  (function project
    "Accesses the project the release is spawned from.

If passed a symbol or string, the project with the given name is
retrieved (if any).

See PROJECT (type)
See PROJECT-RELEASE (type)")
  
  (function release
    "Accesses the release the project-release is a part of.

See RELEASE (type)
See PROJECT-RELEASE (type)")
  
  (function systems
    "Accesses the set of system descriptions that are part of the release.

See SYSTEM (type)
See PROJECT-RELEASE (type)")
  
  (function source-files
    "Accesses the set of source files that are part of the release.

These are pathnames indexing into the project's source-directory.

See PROJECT-RELEASE (type)")

  (function archive-md5
    "Accesses the MD5 hash of the project release's archive.

See PROJECT-RELEASE (type)")

  (function source-sha1
    "Accesses the SHA1 hash of the project release's source files.

See PROJECT-RELEASE (type)")
  
  (function path
    "Returns the local path of the release's source archive.

See PROJECT-RELEASE (type)")
  
  (function prefix
    "Returns the release's prefix.

see PROJECT-RELEASE (type)")
  
  (type system
    "Represents an ASDF system definition.

This is more stable than ASDF:SYSTEMs, as it directly encompasses the
source file the definition is in, the project it stems from, and the
list of all dependencies the system requires to be loaded, without
having to parse (and thus potentially load) the actual system.

See PROJECT
See NAME
See FILE
See DEPENDENCIES")
  
  (function file
    "Accesses the ASD file the system definition was found in.

See SYSTEM (type)")
  
  (function dependencies
    "Accesses the set of systems this system depends on.

The set is a set of system names.

See SYSTEM (type)"))

;;; persistence.lisp
(docs:define-docs
  (variable *distinfo-file*
    "Default location for the distinfo source file.

See PERSIST
See RESTORE")
  
  (function serialize
    "Serialises the given object into an s-expression form.

See PERSIST
See RESTORE")
  
  (function persist
    "Persists the dist/s to disk, to allow reloading of its information in the future.

See *DISTINFO-FILE*
See RESTORE")
  
  (function restore
    "Restores the dist descriptions from the file back into the running image.

Note that this will *update* existing dists, not replace their data.

See *DISTINFO-FILE*
See PERSIST")

  (function define-project
    "Macro to define a project.

The SOURCES should be a list of source-manager descriptions:

  SOURCE-MANAGER ::= (type url initargs*)

The BODY can be a set of initargs for the project, followed by release
descriptions:

  RELEASE ::= (version :systems (SYSTEM*) initargs*)
  SYSTEM  ::= (name initargs*)

If a project of the same name already exists, it is updated.
Note that the name is treated as a case-insensitive string.

See PROJECT (type)
See SOURCE-MANAGER (type)
See PROJECT-RELEASE (type)
See SYSTEM (type)")
  
  (function define-dist
    "Macro to define a dist.

The PROJECTS should be a list of project names to include in the
dist. The BODY can be a set of initargs for the dist, followed by
release descriptions:

  RELEASE ::= (version :projects (PROJECT*))
  PROJECT ::= (name :version version)

If a dist of the same name already exists, it is updated.

See DIST (type)
See RELEASE (type)"))

;;; sources.lisp
(docs:define-docs
  (type cvs
    "Source manager for CVS repositories.

Requires the presence of the cvs binary.

See SOURCE-MANAGER (type)")
  
  (type svn
    "Source manager for SVN repositories.

Requires the presence of the svn binary.

See SOURCE-MANAGER (type)")
  
  (type darcs
    "Source manager for Darcs repositories.

Requires the presence of the darcs binary.

See SOURCE-MANAGER (type)")
  
  (type mercurial
    "Source manager for Mercurial repositories.

Requires the presence of the hg binary.

See SOURCE-MANAGER (type)")
  
  (type git
    "Source manager for Git repositories.

Requires the presence of the git binary.

Accepts additional options:
  :BRANCH -- the specific branch to check out
  :TAG    -- the specific tag to check out

See SOURCE-MANAGER (type)")
  
  (type http
    "Source manager for simple files fetched over HTTP/S.

Requires the presence of the curl binary.

See SOURCE-MANAGER (type)")
  
  (type github
    "Source manager for Github repositories.

Accepts the additional options:
  :TRACK -- May be :RELEASE, in which case it will check out the tag
            of whichever release is latest on Github.

See GIT (type)
See SOURCE-MANAGER (type)")
  
  (type gitlab
    "Source manager for CVS repositories.

Accepts the additional options:
  :TRACK -- May be :RELEASE, in which case it will check out the tag
            of whichever release is latest on Gitlab.
  :TOKEN -- The API token used to access the Gitlab API.

See GIT (type)
See SOURCE-MANAGER (type)")

  (type dist-source
    "Source manager for other quicklisp dists.

Accepts the additional options:
  :PROJECT -- The name of the project in the dist to track.

See SOURCE-MANAGER (type)"))

;;; ql-support.lisp
(docs:define-docs
  (function parse-quicklisp-source
    "Parses a quicklisp source definition.

TYPE should be a symbol designating the type of source, and ARGS
should be a list of strings designating the rest of the arguments on
the source line.

Returns a SOURCE-MANAGER or NIL.

See SOURCE-MANAGER (type)")
  
  (function parse-quicklisp-projects
    "Parse all projects defined in the quicklisp-projects repository and update the dist.

This will also parse the meta files that designate path and system
exclusions.

See PARSE-QUICKLISP-SOURCE-FILE
See DIST (type)")
  
  (function parse-quicklisp-source-file
    "Parse the quicklisp source.txt file that contains source tracking information for a project.

Returns a list of SOURCE-MANAGER instances as parsed from the file.

See PARSE-QUICKLISP-SOURCE
See SOURCE-MANAGER (type)"))

;;; compile.lisp
(docs:define-docs
  (variable *default-output-directory*
    "Standard directory to put dist compilation results.

See COMPILE")
  
  (function compile
    "Compile a dist release.

When called with a DIST, will create a new release of the dist and
compile that. Should this function exit abnormally, the RELEASE
instance will be removed from the DIST again.

When called with a RELEASE, creates a new dist release including
metadata file and any potentially needed source archives for the
projects.

See DIST (type)
See RELEASE (type)"))
