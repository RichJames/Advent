;;; Advent of Code 2015
;;;  Day 4

(require "sb-md5")

(defparameter *key* "iwrupvqb")
(defparameter *key* "abcdef")
(defparameter *key* "pqrstuv")

(defun md5hash (number)
  ;; Returns an md5 hash of number, using key
  ;; MD5 hash returned is in hexadecimal
  (declare (fixnum number))
  (let ((input (concatenate 'string *key* (format nil "~d" number))))
    (md5-calc input)))

(defun md5-calc (string)
  (let* ((md5-digest (sb-md5:md5sum-string string))
	 (md5-digest-list (map 'list #'identity md5-digest)))
    (format nil "~(~{~2,'0x~}~)" md5-digest-list)))

(defun 5-zeros-p (string)
  (let ((first-5 (subseq string 0 5)))
    (string= first-5 "00000")))

(defun 6-zeros-p (string)
  (let ((first-6 (subseq string 0 6)))
    (string= first-6 "000000")))

(defun find-lowest (start-num)
  (do* ((num start-num (1+ num))
	(md5-hash (md5hash num) (md5hash num)))
       ((6-zeros-p md5-hash) (format t "key = ~s, md5-hash = ~s, num =~d" *key* md5-hash num))))
