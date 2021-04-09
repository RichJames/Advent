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

;;; ***** Part2 *****

(defun parse-lines (line1 line2 line3)
  (multiple-value-bind (s11 s21 s31) (parse-line line1)
    (multiple-value-bind (s12 s22 s32) (parse-line line2)
      (multiple-value-bind (s13 s23 s33) (parse-line line3)
        (values s11 s12 s13 s21 s22 s23 s31 s32 s33)))))

(defun part2 ()
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2016/Day03.txt")
    (loop :for line1 = (read-line stream nil nil)
          :for line2 = (read-line stream nil nil)
          :for line3 = (read-line stream nil nil)
          :while line3
          :for (s11 s12 s13 s21 s22 s23 s31 s32 s33) = (multiple-value-list (parse-lines line1 line2 line3))
            :then (multiple-value-list (parse-lines line1 line2 line3))
          :collect (valid-triangle-p s11 s12 s13) :into results
          :collect (valid-triangle-p s21 s22 s23) :into results
          :collect (valid-triangle-p s31 s32 s33) :into results          
          :finally (return (count t results)))))
