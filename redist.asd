#|
 This file is a part of Redist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem redist
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "An extensive system to manage and create quicklisp distributions."
  :homepage "https://Shirakumo.github.io/redist/"
  :bug-tracker "https://github.com/Shirakumo/redist/issues"
  :source-control (:git "https://github.com/Shirakumo/redist.git")
  :build-operation "program-op"
  :build-pathname
  #+windows "redist.exe"
  #+linux "redist.run"
  #-(or windows linux) "redist.o"
  :entry-point "org.shirakumo.dist::main"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "asdf")
               (:file "dist")
               (:file "persistence")
               (:file "sources")
               (:file "ql-support")
               (:file "compile")
               (:file "test")
               (:file "main")
               (:file "documentation"))
  :depends-on (:ironclad
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
               :documentation-utils))
