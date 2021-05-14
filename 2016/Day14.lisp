;;;;
;;;;  Advent of Code 2016
;;;;     Day 14

(defpackage #:2016Day14
  (:use #:cl :cl-ppcre))

(in-package #:2016Day14)

(require "sb-md5")

(defparameter *input* "cuanljph")
(defparameter *generated-hashes* (make-array 1000 :fill-pointer 0 :adjustable t))

(defun reset ()
  (setf *input* "cuanljph"
        (fill-pointer *generated-hashes*) 0))

(defun md5-calc (string)
  (let* ((md5-digest          (sb-md5:md5sum-string string))
         (md5-digest-list     (map 'list #'identity md5-digest)))
    (format nil "~(~{~2,'0x~}~)" md5-digest-list)))

(defun md5hash (number)
  (declare (fixnum number))
  (let ((input (concatenate 'string *input* (format nil "~d" number))))
    (md5-calc input)))

(defun get-md5hash (number)
  (loop :while (<= (fill-pointer *generated-hashes*) number)
        :do (vector-push-extend (md5hash (fill-pointer *generated-hashes*)) *generated-hashes*))

  (elt *generated-hashes* number))

(defun is-key-p (index)
  (let* ((3-char         (ppcre:scan-to-strings "(\\w)(?=\\1{2})" (get-md5hash index)))
         (5-char-regex   (format nil "(~a)(?=\\1{4})" 3-char)))
    (get-md5hash (+ index 1000))
    (position-if #'(lambda (x) (ppcre:scan-to-strings 5-char-regex x)) *generated-hashes* :start (1+ index) :end (+ index 1000))))

(defun find-keys ()
  (loop :with found-keys = 0
        :for i :from 0
        :if (is-key-p i)
          :do (progn
                (incf found-keys)
                (format t "Found key ~a at index ~a~%" found-keys i))
        :while (< found-keys 64)
        :finally (format t "Index ~a produces the 64th key.~%" i)))

(defun part1 ()
  (reset)
  (find-keys))

(defun test-part1 ()
  (reset)
  (setf *input* "abc")
  (find-keys))

