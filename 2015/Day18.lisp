;;;;
;;;; Advent of Code 2015
;;;;   Day 18

(defpackage #:Day18
  (:use #:cl))

(in-package #:Day18)

(defparameter *row-size* 100)
(defparameter *lights* (make-array (* *row-size* *row-size*)))

(defparameter *on* #\#)
(defparameter *off* #\.)

(defparameter *lights-file* "~/quicklisp/local-projects/rich/advent/2015/Day18.txt")

(defun initialize-lights (file)
  (with-open-file (myfile file)
    (loop :with i = 0
          :for char = (read-char myfile nil nil)
          :while char
          :if (or (equal char *on*) (equal char *off*))
            :do (progn
                  (setf (elt *lights* i) char)
                  (incf i)))))

(defun get-row (index)
  (floor (/ index *row-size*)))

(defun get-col (index)
  (- index (* (get-row index) *row-size*)))

(defun get-coords (index)
  (values (get-row index) (get-col index)))

(defun get-index (row col)
  (if (or (< row 0) (>= row *row-size*)
          (< col 0) (>= col *row-size*))
      -1
      (+ (* row *row-size*) col)))

(defun get-light-state (light)
  (if (or (< light 0) (>= light (length *lights*)))
      *off*
      (elt *lights* light)))

(defun get-neighbor-states (light)
  (multiple-value-bind (row col) (get-coords light)
    (let ((n1 (get-light-state (get-index (- row 1) (- col 1))))
          (n2 (get-light-state (get-index (- row 1) col)))
          (n3 (get-light-state (get-index (- row 1) (+ col 1))))
          (n4 (get-light-state (get-index row (- col 1))))
          (n5 (get-light-state (get-index row (+ col 1))))
          (n6 (get-light-state (get-index (+ row 1) (- col 1))))
          (n7 (get-light-state (get-index (+ row 1) col)))
          (n8 (get-light-state (get-index (+ row 1) (+ col 1)))))
      (list n1 n2 n3 n4 n5 n6 n7 n8))))

(defun get-new-light-state (light)
  (let* ((light-state (get-light-state light))
         (neighbor-states (get-neighbor-states light))
         (neighbors-on (count *on* neighbor-states)))
    (cond ((and (equal light-state *on*)
                (or (< neighbors-on 2)
                    (> neighbors-on 3))) *off*)
          ((and (equal light-state *off*)
                (= 3 neighbors-on)) *on*)
          (t light-state))))

(defun update-lights ()
  (let ((new-lights (copy-seq *lights*)))
    (loop :for i :below (length *lights*)
          :do (setf (elt new-lights i) (get-new-light-state i))
          :finally (setf *lights* new-lights))))

(defun animate-lights (steps)
  (loop :repeat steps
        :do (update-lights)))

(defun part1 (steps)
  (initialize-lights *lights-file*)
  (animate-lights steps)
  (count *on* *lights*))


;; Part 2 - just need to redefine get-new-light-state, then re-run part1.
(defun get-new-light-state (light)
  (multiple-value-bind (row col) (get-coords light)
    (let ((min-row-col 0)
          (max-row-col (- *row-size* 1)))
      (cond ((= row col min-row-col) *on*)
            ((= row col max-row-col) *on*)
            ((and (= row min-row-col) (= col max-row-col)) *on*)
            ((and (= row max-row-col) (= col min-row-col)) *on*)
            (t (let* ((light-state (get-light-state light))
                      (neighbor-states (get-neighbor-states light))
                      (neighbors-on (count *on* neighbor-states)))
                 (cond ((and (equal light-state *on*)
                             (or (< neighbors-on 2)
                                 (> neighbors-on 3))) *off*)
                       ((and (equal light-state *off*)
                             (= 3 neighbors-on)) *on*)
                       (t light-state))))))))
