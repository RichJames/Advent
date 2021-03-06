;;;;
;;;; Advent of Code 2015
;;;;   Day 18

(defpackage #:Day18
  (:use #:cl))

(in-package #:Day18)

(defparameter *row-size* 100)

(defparameter *buffer1* (make-array '(100 100)))
(defparameter *buffer2* (make-array '(100 100)))

(defparameter *lights* *buffer1*)
(defparameter *new-lights* *buffer2*)

(defun switch-buffers ()
  (let ((temp *new-lights*))
    (setf *new-lights* *lights*)
    (setf *lights* temp)))

(defparameter *on* #\#)
(defparameter *off* #\.)

(defparameter *lights-file* "~/quicklisp/local-projects/rich/advent/2015/Day18.txt")

(defun initialize-lights (file)
  (setf *lights* *buffer1*)
  (setf *new-lights* *buffer2*)
  (with-open-file (myfile file)
    (loop :with row = 0
          :for line = (read-line myfile nil nil)
          :while line
          :do (progn
                (with-input-from-string (s line)
                 (loop :with col = 0
                       :for light = (read-char s nil nil)
                       :while light
                       :if (or (equal light *on*) (equal light *off*))
                         :do (progn
                               (setf (aref *lights* row col) light)
                               (incf col))))
                (incf row)))))

(defun get-light-state (row col)
  (if (array-in-bounds *lights* row col)
      (aref *lights* row col)
      *off*))

(defun get-neighbor-states (row col)
  (let ((n1 (get-light-state (- row 1) (- col 1)))
        (n2 (get-light-state (- row 1) col))
        (n3 (get-light-state (- row 1) (+ col 1)))
        (n4 (get-light-state row (- col 1)))
        (n5 (get-light-state row (+ col 1)))
        (n6 (get-light-state (+ row 1) (- col 1)))
        (n7 (get-light-state (+ row 1) col))
        (n8 (get-light-state (+ row 1) (+ col 1))))
    (list n1 n2 n3 n4 n5 n6 n7 n8)))

(defun get-part1-new-light-state (row col)
  (let* ((light-state (get-light-state row col))
         (neighbor-states (get-neighbor-states row col))
         (neighbors-on (count *on* neighbor-states)))
    (cond ((and (equal light-state *on*)
                (or (< neighbors-on 2)
                    (> neighbors-on 3))) *off*)
          ((and (equal light-state *off*)
                (= 3 neighbors-on)) *on*)
          (t light-state))))

(defun get-new-light-state (row col)
  (get-part1-new-light-state row col))

(defun update-lights ()
  (loop :for row :below *row-size*
        :do (loop :for col :below *row-size*
                  :do (setf (aref *new-lights* row col) (get-new-light-state row col)))
        :finally (switch-buffers)))

(defun animate-lights (steps)
  (loop :repeat steps
        :do (update-lights)))

(defun part1 (steps)
  (initialize-lights *lights-file*)
  (animate-lights steps)
  (let ((flat-array (make-array (array-total-size *lights*) :displaced-to *lights*)))
    (count *on* flat-array)))

;; Part 2 - just need to redefine get-new-light-state, then re-run part1.

(defun corner-light-p (row col)
  (let ((min-row-col 0)
        (max-row-col (- *row-size* 1)))
    (or (= row col min-row-col)
        (= row col max-row-col)
        (and (= row min-row-col) (= col max-row-col))
        (and (= row max-row-col) (= col min-row-col)))))

(defun get-new-light-state (row col)
  (if (corner-light-p row col)
      *on*
      (get-part1-new-light-state row col)))

