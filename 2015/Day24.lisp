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

(defparameter *sets* (make-hash-table :test 'equal))

(defun find-set (numbers target-number)
  (let ((sorted-numbers  (reverse (sort numbers #'<)))
        (sets            (make-hash-table :test 'equal))
        (original-target target-number))
    
    (labels ((find-set-helper (numbers target-number found-numbers)
               ;;(format t "Numbers are: ~a~%" numbers)
               (cond ((null numbers) nil)
                     ((= (car numbers) target-number) (let ((found-set (push (car numbers) found-numbers)))
                                                        ;;(format t "Finding: ~a~%" found-set)
                                                        (setf (gethash found-set sets) t)
                                                        (find-set-helper (cdr numbers)
                                                                         original-target
                                                                         nil)))
                     ((> (car numbers) target-number) (find-set-helper (cdr numbers)
                                                                       target-number
                                                                       found-numbers))
                     (t (progn
                          (find-set-helper (cdr numbers)
                                           (- target-number (car numbers))
                                           (push (car numbers) found-numbers))
                          (find-set-helper (cdr numbers)
                                           original-target
                                           nil))))))

      (find-set-helper sorted-numbers target-number nil))

    (loop :for key :being :the :hash-keys :of sets
          :collect key)))

(defun find-smallest-packages (numbers target-number)
  (let* ((sets         (find-set numbers target-number))
         (rev-set      (reverse (sort sets #'(lambda (a b) (> (length a) (length b))))))
         (shortest-len (length (car rev-set))))
    ;; need code here to pull only those sets in rev-set that have length shortest-len
    (remove-if-not #'(lambda (a) (= shortest-len (length a))) rev-set)))

(defun quantum-entanglement (list)
  (reduce #'* list))

(defun part1 ()
  (let* ((smallest-packages (find-smallest-packages *packages* *set-weight*))
         (quantum-vals      (mapcar #'quantum-entanglement smallest-packages))
         (sorted-q-vs       (sort quantum-vals #'<)))
    (car sorted-q-vs)))

