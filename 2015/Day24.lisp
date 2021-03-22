;;;;
;;;; Advent of Code 2015
;;;;   Day 24

(defpackage #:Day24
  (:use #:cl))

(in-package #:Day24)

;; We have a list of package weights.
;; We need to find all combinations of three sets of these packages where all
;; 3 sets weigh the same.
;; Then we need to select the set(s) with the fewest packages.
;; Of those, the one with the lowest quantum entanglement is the one we want.
;; We need to find the quantum entanglement of the group with the fewest
;; packages.
;;
;; Quantum entanglement is simply the weights of the packages in that group
;; multiplied together.

(defparameter *packages* '(1 2 3 5 7 13 17 19 23 29 31 37 41 43 53 59 61 67 71
                           73 79 83 89 97 101 103 107 109 113))

(defparameter *set-weight* (/ (reduce #'+ *packages*) 3))

(defun quantum-entanglement (list)
  (reduce #'* list))

(defun sets-2 (l)
  (list (list (car l)) (list (cadr l)) l))

(defun sets-x (l)
  ;; Finds all subsets of set l
  (if (= (length l) 2)
      (sets-2 l)
      (let ((subsets    (sets-x (cdr l)))
            (first-item (car l)))
        (append (list (list first-item)) subsets (mapcar #'(lambda (x) (cons first-item x)) subsets)))))

(defun sets-x (l target)
  ;; Finds all subsets of set l
  (if (= (length l) 2)
      (sets-2 l)
      (let ((subsets    (sets-x (cdr l) target))
            (first-item (car l)))
        (setf subsets (remove-if #'(lambda (x) (> (reduce #'+ x) target)) subsets))
        (append (list (list first-item)) subsets (mapcar #'(lambda (x) (cons first-item x)) subsets)))))

(defparameter *smallest-sets* nil)
(defparameter *smallest-count* most-positive-fixnum)

(defun find-sets-x (l target)
  (if (= (length l) 2)
      (sets-2 l)
      (let ((subsets    (find-sets-x (cdr l) target))
            (first-item (car l)))
        (setf subsets (remove-if #'(lambda (x)
                                     (let ((sum (reduce #'+ x)))
                                       (or (> sum target) (> (+ sum first-item) target))))
                                 subsets))
        (setf subsets (append (list (list first-item)) subsets (mapcar #'(lambda (x) (cons first-item x)) subsets))))))

(defun find-sets-x (l target)
  (if (= (length l) 2)
      (sets-2 l)
      (let* ((subsets    (find-sets-x (cdr l) target))
             (first-item (car l))
             (result     (append (list (list first-item)) subsets (mapcar #'(lambda (x) (cons first-item x)) subsets))))
        (identify-winners result target)
        (remove-if #'(lambda (x) (> (length x) *smallest-count*)) result))))

(defun identify-winners (lists target)
  (loop :for list :in lists
        :do (if (= (reduce #'+ list) target)
                (if (= (length list) *smallest-count*)
                    (push list *smallest-sets*)
                    (if (< (length list) *smallest-count*)
                        (setf *smallest-sets* (list list)
                              *smallest-count* (length list)))))))

(defun find-sets (l target)
  (setf *smallest-sets*  nil
        *smallest-count* most-positive-fixnum)
  (find-sets-x l target)
  (values *smallest-sets*
          *smallest-count*))

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
