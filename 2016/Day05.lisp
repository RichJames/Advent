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
        :when (5-zeros-p hash)
          :collect (progn
                     (incf count)
                     (format t "Found: ~a~%" hash)
                     hash) :into hashes
        :finally (return (mapcar #'(lambda (x) (subseq x 5 6)) hashes))))

;;; ***** Part 2 *****

(defparameter *code* (make-array 8 :initial-element nil))

(defun reset2 ()
  (reset)
  (setf *code* (make-array 8 :initial-element nil)))

(defun update-code (hash)
  (let ((index   (parse-integer (subseq hash 5 6) :junk-allowed t))
        (char-7  (elt hash 6)))
    (if (and index (>= index 0) (< index 8) (not (aref *code* index)))
        (progn
          (format t "Index: ~a, code: ~a~%" index char-7)
          (setf (aref *code* index) char-7)))))

(defun part2 ()
  (reset2)
  (loop :with count = 0
        :for i :from 0
        :until (= count 8)
        :for hash = (md5hash i) :then (md5hash i)
        :when (5-zeros-p hash)
          :if (update-code hash)
            :do (incf count)
        :finally (return (coerce *code* 'string))))
