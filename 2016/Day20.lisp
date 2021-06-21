;;;;
;;;;  Advent of Code 2016
;;;;     Day 20

(defpackage #:2016Day20
  (:use #:cl :cl-ppcre))

(in-package #:2016Day20)

(defun load-input (file)
  (with-open-file (stream file)

    (loop :for ip-range = (read-line stream nil nil)
          :while ip-range
          :collect (mapcar #'(lambda (x) (parse-integer x)) (ppcre:all-matches-as-strings "[0-9]+" ip-range)))))

(defun find-lowest-ip ()
  (let ((ip-ranges (sort (load-input "~/quicklisp/local-projects/rich/advent/2016/Day20.txt") #'< :key #'car)))

    (labels ((find-ip (candidate ip-list)
               (cond ((null ip-list) candidate)
                     ((> candidate (second (car ip-list))) (find-ip candidate (cdr ip-list)))
                     ((<= (first (car ip-list)) candidate (second (car ip-list))) (find-ip (1+ (second (car ip-list))) (cdr ip-list)))
                     (t candidate))))

      (find-ip 0 ip-ranges))))






