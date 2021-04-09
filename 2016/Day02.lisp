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

(defstruct pad-pos
  (x 1) (y 1))

(defparameter *finger-pos* (make-pad-pos))

(defun reset ()
  (setf *finger-pos* (make-pad-pos)))

(defun current-pos ()
  (aref *keypad* (pad-pos-x *finger-pos*) (pad-pos-y *finger-pos*)))

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
  (current-pos))

(defun part1 ()
  (reset)
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2016/Day02.txt")
    (loop :for line = (read-line stream nil nil)
          :while line
          :collect (process-line line))))

;;; ***** Part 2 *****

(defun reset ()
  (setf *keypad* (make-array 13 :initial-contents
                             '(    1
                                 2 3 4
                               5 6 7 8 9
                                 A B C
                                   D))
        *finger-pos* 4))

(defun current-pos ()
  (aref *keypad* *finger-pos*))

(defun move-up ()
  (if (not (member *finger-pos* '(0 1 3 4 8)))
      (cond ((= *finger-pos*  2) (setf *finger-pos*  0))
            ((= *finger-pos* 12) (setf *finger-pos* 10))
            (t (setf *finger-pos* (- *finger-pos* 4))))))

(defun move-down ()
  (if (not (member *finger-pos* '(4 8 9 11 12)))
      (cond ((= *finger-pos*  0) (setf *finger-pos*  2))
            ((= *finger-pos* 10) (setf *finger-pos* 12))
            (t (setf *finger-pos* (+ *finger-pos* 4))))))

(defun move-right ()
  (if (not (member *finger-pos* '(0 3 8 11 12)))
      (setf *finger-pos* (1+ *finger-pos*))))

(defun move-left ()
  (if (not (member *finger-pos* '(0 1 4 9 12)))
      (setf *finger-pos* (1- *finger-pos*))))

(defun part2 ()
  (part1))
