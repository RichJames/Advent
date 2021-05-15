;;;;
;;;;  Advent of Code 2016
;;;;     Day 15

(defpackage #:2016Day15
  (:use #:cl))

(in-package #:2016Day15)

(defparameter *discs* #((13 1) (19 10) (3 2) (7 1) (5 3) (17 5)))

(defun test-advance-disc (disc-idx steps)
  (let ((slots     (car (elt *discs* disc-idx)))
        (curr-slot (cadr (elt *discs* disc-idx))))
    (mod (+ curr-slot steps) slots)))

(defun advance-discs ()
  (loop :for i :below (array-total-size *discs*)
        :do (setf (cadr (elt *discs* i)) (mod (incf (cadr (elt *discs* i))) (car (elt *discs* i))))))

(defun slots-aligned-p ()
  (loop :with aligned = t
        :for i :below (array-total-size *discs*)
        :if (not (zerop (test-advance-disc i i)))
          :do (setf aligned nil)
        :finally (return aligned)))

(defun find-aligned-slots ()
  (loop :for i :from 0
        :do (advance-discs)
        :until (slots-aligned-p)
        :finally (return i)))

(defun part1 ()
  (find-aligned-slots))

;;; ***** Part 2 *****

(defparameter *discs* #((13 1) (19 10) (3 2) (7 1) (5 3) (17 5) (11 0)))

;;; Re-run part1() after compiling the new *discs* definition above to get the answer.
