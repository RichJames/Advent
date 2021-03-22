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
  (cond ((= col row 1) (list row col))
        ((= col 1) (list 1 (- row 1)))
        (t (list (+ row 1) (- col 1)))))

(defun next-cell (row col)
  (cond ((= row col 1) (list 2 1))
        ((= row 1) (list (+ row col) 1))
        (t (list (- row 1) (+ col 1)))))

(defun find-matrix-size (row col)
  (loop :with max-row = row :and max-col = col
        :for (r c) = (prior-cell row col) :then (prior-cell r c)
        :do (progn
              (if (> r max-row) (setf max-row r))
              (if (> c max-col) (setf max-col c)))
        :until (= c 1)
        :finally (let ((max-dim (max max-row max-col)))
                   (return (list max-dim max-dim)))))

(defun get-code-old (cell)
  (let ((multiplier   252533)
        (divisor    33554393)
        (row        (car cell))
        (col        (cadr cell)))
    (cond ((= row col 1) *first-code*)
          (t (rem (* (get-code (prior-cell row col)) multiplier) divisor)))))


(defun get-code (row col)
  (let* ((matrix-dims  (find-matrix-size row col))
         (matrix-rows  (car matrix-dims))
         (matrix-cols  (cadr matrix-dims))
         (codes    (make-array matrix-dims)))

    (setf (aref codes 0 0) *first-code*)

    (loop :for (r c) = (next-cell 1 1) :then (next-cell r c)
          :until (or (> r matrix-rows) (> c matrix-cols))          
          :do (let* ((prev-cell (prior-cell r c))
                     (prev-row  (car prev-cell))
                     (prev-col  (cadr prev-cell))
                     (prev-code (aref codes (1- prev-row) (1- prev-col))))
                (setf (aref codes (1- r) (1- c)) (next-code prev-code))))

    (aref codes (1- row) (1- col))))


#|
(1 1) ==> initial value      **** special case
(2 1) ==> (1 1)      (-1  0) *** (col = 1)
(1 2) ==> (2 1)      (+1 -1)
(3 1) ==> (1 2)      (-2 +1) *** (col = 1): set col = row - 1, set row = 1
(2 2) ==> (3 1)      (+1 -1)
(1 3) ==> (2 2)      (+1 -1)
(4 1) ==> (1 3)      (-3 +2) *** (col = 1)
(3 2) ==> (4 1)      (+1 -1)
(2 3) ==> (3 2)      (+1 -1)
(1 4) ==> (2 3)      (+1 -1)
(5 1) ==> (1 4)      (-4 +3) *** (col = 1)
(4 2) ==> (5 1)      (+1 -1)
(3 3) ==> (4 2)      (+1 -1)
(2 4) ==> (3 3)      (+1 -1)
(1 5) ==> (2 4)      (+1 -1)
(6 1) ==> (1 5)      (-5 +4) *** (col = 1)

(7 1) ==> (1 6)
(5 1) ==> (1 4)
-------------------------------------------
(1 1) ==> (2 1)   (+1  0) ***
(2 1) ==> (1 2)   (-1 +1)
(1 2) ==> (3 1)   (+2 -1) *** (input row = 1), row = row + col, col = 1
(3 1) ==> (2 2)   (-1 +1)
(2 2) ==> (1 3)   (-1 +1)
(1 3) ==> (4 1)   (+3 -2) ***
(4 1) ==> (3 2)   (-1 +1)
(3 2) ==> (2 3)   (-1 +1)
(2 3) ==> (1 4)   (-1 +1)
(1 4) ==> (5 1)   (+4 -3) ***
|#
