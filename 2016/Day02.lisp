;;;;
;;;;  Advent of Code 2016
;;;;     Day 02

(defpackage #:Day02
  (:use #:cl))

(in-package #:Day02)

(defparameter *keypad* (make-array '(3 3) :initial-contents
                                   '((1 2 3)
                                     (4 5 6)
                                     (7 8 9))))
(defparameter *code* nil)

(defstruct pad-pos
  (x 1) (y 1))

(defparameter *finger-pos* (make-pad-pos))

(defun reset ()
  (setf *finger-pos* (make-pad-pos)))

(defun move-up ()
  (if (/= (pad-pos-x *finger-pos*) 0)
      (setf (pad-pos-x *finger-pos*) (1- (pad-pos-x *finger-pos*)))))

(defun move-down ()
  (let ((upper-limit (1- (array-dimension *keypad* 0))))
    (if (/= (pad-pos-x *finger-pos*) upper-limit)
        (setf (pad-pos-x *finger-pos*) (1+ (pad-pos-x *finger-pos*))))))

(defun move-right ()
  (let ((upper-limit (1- (array-dimension *keypad* 1))))
    (if (/= (pad-pos-y *finger-pos*) upper-limit)
        (setf (pad-pos-y *finger-pos*) (1+ (pad-pos-y *finger-pos*))))))

(defun move-left ()
  (if (/= (pad-pos-y *finger-pos*) 0)
      (setf (pad-pos-y *finger-pos*) (1- (pad-pos-y *finger-pos*)))))

(defun process-line (line)
  (loop :for c :across line
        :do (cond ((equal c #\U) (move-up))
                  ((equal c #\D) (move-down))
                  ((equal c #\R) (move-right))
                  ((equal c #\L) (move-left))))
  (aref *keypad* (pad-pos-x *finger-pos*) (pad-pos-y *finger-pos*)))


(defun part1 ()
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2016/Day02.txt")
    (loop :for line = (read-line stream nil nil)
          :while line
          :collect (process-line line))))
