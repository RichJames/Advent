;;;;
;;;; Advent of Code 2015
;;;;   Day 17

(defpackage #:Day17
  (:use #:cl))

(in-package #:Day17)

;; This is a subset sum problem
;; https://en.wikipedia.org/wiki/Subset_sum_problem

;; I uniquely name the containers to avoid issues where some have identical
;; capacities.
(defparameter *containers* '((a . 50) (b . 44) (c . 11) (d . 49) (e . 42) (f . 46) (g . 18)
                             (h . 32) (i . 26) (j . 40) (k . 21) (l . 7) (m . 18) (n . 43)
                             (o . 10) (p . 47) (q . 36) (r . 24) (s . 22) (t . 40)))

(defparameter *unused-containers* '(a b c d e f g h i j k l m n o p q r s t))
(defparameter *used-containers* nil)
(defparameter *tried-containers* nil)
(defparameter *candidates* nil)
(defparameter *found-combos* nil)

(defun save-candidate (container)
  (setf *candidates* (append *candidates* (list container))))

(defun find-candidates (containers total)
  (setf *candidates* nil)
  (loop :for container :in containers
        :if (<= (cdr (assoc container *containers*)) total)
          :do (save-candidate container)
        :finally (return *candidates*)))

(defmacro move-container-from-to (container from to)
  `(progn
     (setf ,to (append ,to (list ,container)))
     (setf ,from (remove-if #'(lambda (x) (equal x ,container)) ,from))))

(defun use-container (container)
  (move-container-from-to container *unused-containers* *used-containers*)
  (sum-containers *used-containers*))

(defun return-container (container)
  (flet ((container-liters (container)
           (cdr (assoc container *containers*))))
    (let ((reusable-containers (remove-if-not #'(lambda (x) (< (container-liters x)
                                                               (container-liters container)))
                                              *tried-containers*)))
      (move-container-from-to container *used-containers* *tried-containers*)
      (loop :for reusable :in reusable-containers
            :do (move-container-from-to reusable *tried-containers* *unused-containers*))
      (sum-containers *used-containers*))))

(defun sum-containers (containers)
  (reduce #'+ (mapcar #'(lambda (x) (cdr (assoc x *containers*))) containers)))

(defun reset-containers ()
  (setf *unused-containers* '(a b c d e f g h i j k l m n o p q r s t))
  (setf *used-containers* nil)
  (setf *tried-containers* nil)
  (setf *candidates* nil)
  (setf *found-combos* nil))

(defun save-combo (combo)
  (let ((sorted-combo (sort combo #'string-lessp)))
    (if (not (member sorted-combo *found-combos* :test 'equal))
        (setf *found-combos* (append *found-combos* (list sorted-combo))))))

(defun find-combos (total)
  (reset-containers)
  (loop :while (or *unused-containers* *used-containers*)
        :do (let* ((to-fill (- total (sum-containers *used-containers*)))
                   (candidate (car (find-candidates *unused-containers* to-fill))))
              (if candidate
                  (if (= (use-container candidate) total)
                      (progn
                        (save-combo *used-containers*)
                        (return-container (car (reverse *used-containers*)))))
                  (return-container (car (reverse *used-containers*)))))
        :finally (return (values (length *found-combos*)
                                 *found-combos*))))

(defun foo (containers total)
  (loop :for container :in containers
        :if (<= (cdr (assoc container *containers*)) total)
          :collect container
        :else
          :do (foo (cdr containers) (- total (cdr (assoc container *containers*))))))

(defun foo (containers total)
  (let ((container (car containers)))
    (cond ((null containers) nil)
          ((> (cdr container) total) (foo (cdr containers) total))
          ((= (cdr container) total) container)
          (t (foo (cdr containers) (- total (cdr container)))))))

(defun subsetSum (set subSet n subSize total nodeCount sum)
  (if (= total sum)
      (progn
        (print subSet)
        (subsetSum set subSet n (1- subSize) (- total (nth nodeCount set)) (1+ nodeCount sum)))
      (subsetSum set (cdr set) n )))

(defun subsetSum (set sum &optional (candidates nil))
  (cond ((zerop sum) candidates)
        ((= 1 (length set)) (if (= sum (car set))
                                (append candidates set)
                                nil))
        (t (flatten (list (subsetSum (cdr set)
                                     (- sum (car set))
                                     (append candidates (list (car set))))
                          (subsetSum (cdr set)
                                     sum
                                     candidates))))))

(defun subsetSum (set sum found-combos &optional (candidates nil))
  (cond ((null set))
        ((zerop sum) (vector-push candidates found-combos))
        ((and (= 1 (length set)) (= sum (car set))) (vector-push (append candidates set) found-combos))
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

