(asdf:defsystem redist
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "An extensive system to manage and create quicklisp distributions."
  :homepage "https://Shirakumo.github.io/redist/"
  :bug-tracker "https://github.com/Shirakumo/redist/issues"
  :source-control (:git "https://github.com/Shirakumo/redist.git")
  :build-operation "program-op"
  :build-pathname
  #+windows "redist.exe"
  #+darwin "redist.o"
  #-(or darwin windows) "redist"
  :entry-point "org.shirakumo.redist::main"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "asdf")
               (:file "storage")
               (:file "dist")
               (:file "project")
               (:file "release")
               (:file "plaintext")
               (:file "sqlite")
               (:file "sources")
               (:file "ql-support")
               (:file "compile")
               (:file "replicate")
               (:file "test")
               (:file "main")
               (:file "documentation"))
  :depends-on (:ironclad
               :closer-mop
               :shasht
               :babel
               :archive
               :salza2
               :eclector
               :simple-inferiors
               :pathname-utils
               :filesystem-utils
               :uiop
               :clip
               :form-fiddle
               :documentation-utils
               :lparallel
               :cl-ppcre
               :sqlite))
