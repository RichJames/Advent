;;;;
;;;; Advent of Code 2015
;;;;   Day 24

(defpackage #:Day24
  (:use #:cl))

(in-package #:Day24)

(defparameter *packages* '(1 2 3 5 7 13 17 19 23 29 31 37 41 43 53 59 61 67 71
                           73 79 83 89 97 101 103 107 109 113))

(defun quantum-entanglement (list)
  (reduce #'* list))

(defun sets-2 (l)
  (list (list (car l)) (list (cadr l)) l))

(defparameter *smallest-sets* nil)
(defparameter *smallest-count* most-positive-fixnum)

(defun identify-winners (lists target)
  (loop :for list :in lists
        :do (if (= (reduce #'+ list) target)
                (if (= (length list) *smallest-count*)
                    (push list *smallest-sets*)
                    (if (< (length list) *smallest-count*)
                        (setf *smallest-sets* (list list)
                              *smallest-count* (length list)))))))

(defun find-sets-x (l target)
  (if (= (length l) 2)
      (sets-2 l)
      (let* ((subsets    (find-sets-x (cdr l) target))
             (first-item (car l))
             (result     (append (list (list first-item)) subsets (mapcar #'(lambda (x) (cons first-item x)) subsets))))
        (identify-winners result target)
        (remove-if #'(lambda (x) (> (length x) *smallest-count*)) result))))

(defun find-sets (l target)
  (setf *smallest-sets*  nil
        *smallest-count* most-positive-fixnum)
  (find-sets-x l target)
  *smallest-sets*)

(defun find-best-q-e (l target)
  (let* ((sets              (find-sets l target))
         (quantum-vals      (mapcar #'quantum-entanglement sets))
         (sorted-q-vs       (sort quantum-vals #'<)))
    (car sorted-q-vs)))

(defun part1 ()
  (let* ((weight (/ (reduce #'+ *packages*) 3)))
    (find-best-q-e *packages* weight)))

(defun part2 ()
  (let* ((weight (/ (reduce #'+ *packages*) 4)))
    (find-best-q-e *packages* weight)))
