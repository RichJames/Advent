;;;;
;;;; Advent of Code 2015
;;;;   Day 17

(defpackage #:Day17
  (:use #:cl))

(in-package #:Day17)

;; This is a subset sum problem
;; https://en.wikipedia.org/wiki/Subset_sum_problem

(defparameter *containers* '(50 44 11 49 42 46 18 32 26 40 21 7 18 43 10 47 36 24 22 40))

(defun subsetSum (set sum found-combos &optional (candidates nil))
  (cond ((null set))
        ((zerop sum) (vector-push-extend candidates found-combos))
        ((and (= 1 (length set))
              (= sum (car set))) (vector-push-extend (append candidates set) found-combos))
        (t (progn
             (subsetSum (cdr set)
                        (- sum (car set))
                        found-combos
                        (append candidates (list (car set))))
             (subsetSum (cdr set)
                        sum
                        found-combos
                        candidates)))))

(defun find-combos (set sum)
  (let ((found-combos (make-array 10 :fill-pointer 0 :adjustable t)))
    (subsetSum set sum found-combos)
    (loop :for x :across found-combos
          :do (print x))
    (length found-combos)))


;; Part 2

;; Find the smallest set of containers that can hold 150 liters.  How many sets of
;; that many containers are there?

(defun find-part2-combos (set sum)
  (let ((found-combos (make-array 10 :fill-pointer 0 :adjustable t)))
    (subsetSum set sum found-combos)
    (let ((sorted-lengths (sort (loop :for set :across found-combos
                                      :collect (length set)) #'<)))
      (count (car sorted-lengths) sorted-lengths))))
