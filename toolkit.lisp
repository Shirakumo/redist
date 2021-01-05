#|
 This file is a part of Shirakumo-Dist
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defun tar (files output &key (if-exists :error))
  (archive:with-open-archive (archive output
                              :direction :output
                              :if-exists if-exists)
    (dolist (file files)
      (let ((entry (archive:create-entry-from-pathname archive file)))
        (archive:write-entry-to-archive archive entry)))
    (archive:finalize-archive archive)
    output))

(defun gz (file output &key (if-exists :error))
  (salza2:gzip-file file output :if-exists if-exists))

(defun tgz (files output &key (if-exists :error))
  (let ((tar (make-pathname :type "tar" :defaults output)))
    (tar files tar :if-exists :error)
    (unwind-protect
         (gz tar output :if-exists if-exists)
      (delete-file tar))))

(defun enlist (list &rest els)
  (if (listp list) list (list* list els)))

(defun digest (file/s digest)
  (let ((digest (ironclad:make-digest digest)))
    (dolist (file (enlist file/s) (ironclad:byte-array-to-hex-string
                                   (ironclad:produce-digest digest)))
      (with-open-file (stream file :element-type '(unsigned-byte 8))
        (ironclad:update-digest digest stream)))))

(defun file-size (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (file-length stream)))
