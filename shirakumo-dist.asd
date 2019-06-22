#|
 This file is a part of Shirakumo-Dist
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem shirakumo-dist
  :name "Shirakumo-Dist"
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A toolkit to handle the Shirakumo Quicklisp dist."
  :homepage "https://Shirakumo.github.io/dist/"
  :bug-tracker "https://github.com/Shirakumo/dist/issues"
  :source-control (:git "https://github.com/Shirakumo/dist.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "git")
               (:file "sources")
               (:file "dist")
               (:file "test"))
  :depends-on (:quickdist
               :hunchentoot))
