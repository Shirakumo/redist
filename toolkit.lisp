#|
 This file is a part of Shirakumo-Dist
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.dist)

(defun splice-sublists (list)
  "Splices all lists in the list into one big list."
  (loop for item in list
        appending (if (listp item)
                      item
                      (list item))))

(defun timestamp (&optional (time (get-universal-time)))
  "Returns a timestamp in the following format:
YYYY.MM.DD-hh:mm:ss-GMT"
  (format NIL "~:@{~4,'0d.~2,'0d.~2,'0d_~2,'0d-~2,'0d-~2,'0d~}-GMT"
          (subseq (nreverse (multiple-value-list (decode-universal-time time 0))) 3)))

(defun search* (thing place)
  "Performs SEARCH with the test set to CHAR-EQUAL."
  (search thing place :test #'char-equal))

(defun ask-purge-directory (dir)
  "An interactive prompt that when answered with Y will delete the given directory tree.
Only the directory's contents are deleted, not the directory itself."
  (format T "~&Are you absolutely sure [y/n]? ")
  (when (string-equal (read-line) "y")
    (progn (uiop:delete-directory-tree dir :validate (constantly T))
           (ensure-directories-exist dir))))
