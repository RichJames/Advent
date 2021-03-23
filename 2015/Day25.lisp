;;;;
;;;; Advent of Code 2015
;;;;   Day 25

(defpackage #:Day25
  (:use #:cl))

(in-package #:Day25)

;; To continue, please consult the code grid in the manual.  Enter the code at row 2978, column 3083.

(defparameter *first-code* 20151125)

(defun next-code (code)
  (let ((multiplier   252533)
        (divisor    33554393))
    (rem (* code multiplier) divisor)))

(defun prior-cell (row col)
  (cond ((= col row 1) (values row col))
        ((= col 1)     (values 1 (- row 1)))
        (t             (values (+ row 1) (- col 1)))))

(defun next-cell (row col)
  (cond ((= row col 1) (values 2 1))
        ((= row 1)     (values (+ row col) 1))
        (t             (values (- row 1) (+ col 1)))))

(defun find-matrix-size (row col)
  (cond ((= row col 1)             (values row col))
        ((= col 1)                 (values row (nth-value 1 (prior-cell row col))))
        ((= row 1)                 (values col col))
        (t (loop :for (r c) = (multiple-value-list (prior-cell row col))
                   :then (multiple-value-list (prior-cell r c))
                 :when (= c 1)
                   :return (values r (1- r))))))

(defun get-code (row col)
  (multiple-value-bind (matrix-rows matrix-cols) (find-matrix-size row col)
    (let ((codes  (make-array (list matrix-rows matrix-cols))))

      (setf (aref codes 0 0) *first-code*)

      (loop :for (r c) = (multiple-value-list (next-cell 1 1))
              :then (multiple-value-list (next-cell r c))
            :until (or (> r matrix-rows) (> c matrix-cols))          
            :do (multiple-value-bind (prev-row prev-col) (prior-cell r c)
                  (let ((prev-code (aref codes (1- prev-row) (1- prev-col))))
                    (setf (aref codes (1- r) (1- c)) (next-code prev-code)))))

      (aref codes (1- row) (1- col)))))

(defun part1 ()
  (get-code 2978 3083))








