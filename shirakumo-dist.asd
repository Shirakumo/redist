#|
 This file is a part of Shirakumo-Dist
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(asdf:defsystem shirakumo-dist
  :name "Shirakumo-Dist"
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A toolkit to handle the Shirakumo Quicklisp dist."
  :homepage "https://github.com/Shirakumo/dist"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "git")
               (:file "sources")
               (:file "dist")
               (:file "test"))
  :depends-on (:quickdist
               :hunchentoot))
