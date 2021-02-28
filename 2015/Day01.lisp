;;;;
;;;; Advent of Code 2015
;;;;   Day 01

(defpackage #:Day01
  (:use #:cl))

(in-package #:Day01)

(defun which-floor (commands)
	   (- (count #\( commands)
	      (count #\) commands)))

(defun basement (commands)
  (let ((floor 0)
	(pos 0))
    (loop :for c :in (coerce commands 'list)
	  :do (progn
		(cond ((equal c #\() (incf floor))
		      ((equal c #\)) (decf floor)))
		(incf pos))
	  :when (= floor -1)
	    :return pos
	  :finally (format t "~%floor = ~s" floor))))
