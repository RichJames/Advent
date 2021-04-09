;;;;
;;;;  Advent of Code 2016
;;;;     Day 03

(defpackage #:Day03
  (:use #:cl))

(in-package #:Day03)

(defun valid-triangle-p (s1 s2 s3)
  (and (> (+ s1 s2) s3)
       (> (+ s1 s3) s2)
       (> (+ s2 s3) s1)))

(defun parse-line (line)
  (with-input-from-string (s line)
    (let ((s1 (read s nil nil))
          (s2 (read s nil nil))
          (s3 (read s nil nil)))
      (values s1 s2 s3))))

(defun part1 ()
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2016/Day03.txt")
    (loop :for line = (read-line stream nil nil)
          :while line
          :collect (multiple-value-bind (s1 s2 s3) (parse-line line)
                     (valid-triangle-p s1 s2 s3)) :into results
          :finally (return (count t results)))))
