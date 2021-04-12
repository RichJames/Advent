;;;;
;;;;  Advent of Code 2016
;;;;     Day 07

(defpackage #:2016Day07
  (:use #:cl #:cl-ppcre))

(in-package #:2016Day07)

(defparameter *abba-regex* "(.)(?!\\1)(.)\\2\\1")
(defparameter *hypernet-regex* "\\[.+?\\]")

(defun get-hypernet (ip)
  (ppcre:all-matches-as-strings *hypernet-regex* ip))

(defun abba-p (string)
  (nth-value 0 (ppcre:scan-to-strings *abba-regex* string)))

(defun tls-p (ip)
  (let ((hypernet-list (get-hypernet ip)))
    (loop :with tls = (abba-p ip)
          :for hypernet :in hypernet-list
          :if (abba-p hypernet)
            :do (setf tls nil)
          :finally (return tls))))

(defun part1 ()
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2016/Day07.txt")
    (loop :for ip = (read-line stream nil nil)
          :while ip
          :if (tls-p ip)
            :count ip)))
