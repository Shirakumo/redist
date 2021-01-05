#|
 This file is a part of Shirakumo-Dist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem shirakumo-dist
  :version "2.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple system to set up quicklisp dists from git repos."
  :homepage "https://Shirakumo.github.io/dist/"
  :bug-tracker "https://github.com/Shirakumo/dist/issues"
  :source-control (:git "https://github.com/Shirakumo/dist.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "asdf")
               (:file "dist")
               (:file "compile"))
  :depends-on (:legit
               :ironclad
               :archive
               :salza2
               :eclector))
