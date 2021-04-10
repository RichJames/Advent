;;;;
;;;;  Advent of Code 2016
;;;;     Day 05

(defpackage #:2016Day05
  (:use #:cl))

(in-package #:2016Day05)

(require "sb-md5")

(defparameter *input* "ffykfhsq")

(defun reset ()
  (setf *input* "ffykfhsq"))

(defun md5-calc (string)
  (let* ((md5-digest          (sb-md5:md5sum-string string))
         (md5-digest-list     (map 'list #'identity md5-digest)))
    (format nil "~(~{~2,'0x~}~)" md5-digest-list)))

(defun md5hash (number)
  (declare (fixnum number))
  (let ((input (concatenate 'string *input* (format nil "~d" number))))
    (md5-calc input)))

(defun 5-zeros-p (string)
  (let ((first-5 (subseq string 0 5)))
    (string= first-5 "00000")))

(defun part1 ()
  (reset)
  (loop :with count = 0
        :for i :from 0
        :until (= count 8)
        :for hash = (md5hash i) :then (md5hash i)
        :when (5-zeros-p (md5hash i))
          :collect (progn
                     (incf count)
                     (format t "Found: ~a~%" hash)
                     hash) :into hashes
        :finally (return (mapcar #'(lambda (x) (subseq x 5 6)) hashes))))
