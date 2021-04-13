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

;;; ***** Part2 *****

(defparameter *aba-regex* "([a-zA-Z])(?!\\1)([a-zA-Z])\\1")

(defun get-aba (ip)
  "Returns list of aba sequences in ip that occur outside of hypernet sequences."
  (let ((all-aba-list  (get-all-aba ip))
        (hypernet-list (get-hypernet ip)))
    (loop :with final-aba-list = all-aba-list
          :for hypernet :in hypernet-list
          :for hypernet-aba = (get-all-aba hypernet)
          :do (loop :for hyper-aba :in hypernet-aba
                    :do (setf final-aba-list (remove hyper-aba final-aba-list :test #'equal :count 1)))
          :finally (return final-aba-list))))

(defun get-all-aba (ip)
  "Returns list of all aba sequences in ip, regardless of where they occur."
  (loop :with s = 0
        :with e = (length ip)
        :for m = (multiple-value-bind (start end) (ppcre:scan *aba-regex* ip :start s :end e)
                   (if (and start end)
                       (progn
                         (setf s (1+ start))
                         (subseq ip start end))))
        :while m
        :collect m))

(defun build-bab (aba)
  (let ((first  (char aba 0))
        (second (char aba 1)))
    (format nil "~a~a~a" second first second)))

(defun ssl-p (ip)
  (let ((hypernet-list (get-hypernet ip))
        (aba-list      (get-aba ip)))
    (loop :with ssl = nil
          :for aba :in aba-list
          :for bab = (build-bab aba)
          :do (loop :for hypernet :in hypernet-list
                    :if (search bab hypernet)
                      :do (setf ssl t))
          :finally (return ssl))))

(defun part2 ()
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2016/Day07.txt")
    (loop :for ip = (read-line stream nil nil)
          :while ip
          :if (ssl-p ip)
            :count ip)))


