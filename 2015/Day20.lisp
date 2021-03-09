;;;;
;;;; Advent of Code 2015
;;;;   Day 20

(defpackage #:Day20
  (:use #:cl))

(in-package #:Day20)

;; A given elf, x, delivers (* 10 x) presents to every x houses.  This first elf is number 1.
;; There are potentially an infinite number of houses.

;; Problem: find the lowest house number that recieved at least 29000000 presents.

(defparameter *min-presents* 29000000)

(defun visit-houses (min-presents)
  (let* ((houses (make-array (/ min-presents 10) :initial-element 0))
         (house-array-size (array-total-size houses)))
    (loop :for i :below house-array-size
          :do (loop :for j :from i :below house-array-size :by (+ i 1)
                    :for house-current-total = (aref houses j)
                    :do (setf (aref houses j) (+ house-current-total (* (+ i 1) 10))))
          :finally (return houses))))

(defun find-lowest-house-number (min-presents)
  (let ((houses (visit-houses min-presents)))
    (+ 1 (position min-presents houses :test #'(lambda (i x) (>= x i))))))

;; Part 2

;; Elves now only visit up to 50 houses.  Now, for a given elf, x, they deliver
;; (* 11 x) presents to every x houses.  What is the new low house number?

(defun part2-visit-houses (min-presents visit-limit)
  (let* ((houses (make-array (/ min-presents 10) :initial-element 0))
         (house-array-size (array-total-size houses)))
    (loop :for i :below house-array-size
          :for step = (+ i 1)
          :do (loop :for j :from i :below (min (* step visit-limit) house-array-size) :by step
                    :for house-current-total = (aref houses j)
                    :do (setf (aref houses j) (+ house-current-total (* step 11))))
          :finally (return houses))))

(defun part2-find-lowest-house-number (min-presents)
  (let ((houses (part2-visit-houses min-presents 50)))
    (+ 1 (position min-presents houses :test #'(lambda (i x) (>= x i))))))
