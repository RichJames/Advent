;;;;
;;;;  Advent of Code 2016
;;;;     Day 06

(defpackage #:2016Day06
  (:use #:cl))

(in-package #:2016Day06)

(defparameter *letter-counts* (make-array '(8 26) :initial-element 0))

(defparameter *input* "~/quicklisp/local-projects/rich/advent/2016/Day06.txt")

(defun reset ()
  (setf *letter-counts* (make-array '(8 26) :initial-element 0)))

(defun count-letters (line)
  (loop :with code-for-a = (char-code #\a)
        :for i :upto (length line)
        :for l :across line
        :for l-code = (- (char-code l) code-for-a)
        :do (incf (aref *letter-counts* i l-code))))

(defun count-input (file)
  (reset)
  (with-open-file (stream file)
    (loop :for line = (read-line stream nil nil)
          :while line
          :do (count-letters line))))

(defun decode-input ()
  (loop :with message = nil
        :for i :below 8
        :do (loop :with code-for-a = (char-code #\a)
                  :for j :below 26
                  :for count = (aref *letter-counts* i j)
                  :for c-code = code-for-a :then (1+ c-code)
                  :for letter = (code-char c-code)
                  :collect (list letter count) :into results
                  :finally (push (list (caar (stable-sort results #'> :key 'cadr))) message))
        :finally (return (coerce (mapcar #'car (reverse message)) 'string))))

(defun part1 ()
  (reset)
  (count-input *input*)
  (decode-input))

;;; ***** Part 2 *****

(defun decode-input2 ()
  (loop :with message = nil
        :for i :below 8
        :do (loop :with code-for-a = (char-code #\a)
                  :for j :below 26
                  :for count = (aref *letter-counts* i j)
                  :for c-code = code-for-a :then (1+ c-code)
                  :for letter = (code-char c-code)
                  :if (> count 0)
                    :collect (list letter count) :into results
                  :finally (push (list (caar (stable-sort results #'< :key 'cadr))) message))
        :finally (return (coerce (mapcar #'car (reverse message)) 'string))))

(defun part2 ()
  (reset)
  (count-input *input*)
  (decode-input2))
