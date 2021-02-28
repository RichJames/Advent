;;;;
;;;; Advent of Code 2015
;;;;   Day 14

(defpackage #:Day14
  (:use #:cl))

(in-package #:Day14)

(defparameter *raw-data*
  '(("Vixen"   (19 7 124))
    ("Rudolph" (3 15 28))
    ("Donner"  (19 9 164))
    ("Blitzen" (19 9 158))
    ("Comet"   (13 7 82))
    ("Cupid"   (25 6 145))
    ("Dasher"  (14 3 38))
    ("Dancer"  (3 16 37))
    ("Prancer" (25 6 143))))

(defparameter *test-data*
  '(("Comet"  (14 10 127))
    ("Dancer" (16 11 162))))

(defclass reindeer ()
  ((name      :initarg :name      :accessor name)
   (rate      :initarg :rate      :accessor rate)
   (duration  :initarg :duration  :accessor duration)
   (rest-time :initarg :rest-time :accessor rest-time)))

(defparameter *reindeers* (make-hash-table :test 'equal))

(defun populate-reindeers (data)
  (clrhash *reindeers*)
  (loop :for r :in data
        :do (setf (gethash (first r) *reindeers*) (make-instance 'reindeer
                                                                 :name (first r)
                                                                 :rate (first (cadr r))
                                                                 :duration (second (cadr r))
                                                                 :rest-time (third (cadr r))))))

(defun distance-travelled (reindeer seconds)
  (let* ((duration        (duration reindeer))
         (distance        (* (rate reindeer) duration))
         (cycle-time      (+ duration (rest-time reindeer)))
         (cycles          (floor (/ seconds cycle-time)))
         (cycles-distance (* cycles distance))
         (remaining-time  (mod seconds cycle-time)))
    (if (> remaining-time duration)
        (+ distance cycles-distance)
        cycles-distance)))

(defun find-winner (seconds)
  (loop :with best-distance = 0
        :with winner = 'None
        :for k :being :each :hash-key :of *reindeers* :using (hash-value reindeer)
        :do (let ((distance (distance-travelled reindeer seconds)))
              (if (> distance best-distance)
                  (progn
                    (setf best-distance distance)
                    (setf winner (name reindeer)))))
        :finally (return (values winner best-distance))))

;; Part 2 - score by seconds

;; Given a duration in seconds, calculate the position of each reindeer after each second.
;; Award a point to each reindeer in the lead at that moment.  The winner is the one with
;; the most points.

(defparameter *scores* (make-hash-table :test 'equal))

(defun init-scores-table (data)
  (clrhash *scores*)
  (loop :for r :in data
        :do (setf (gethash (first r) *scores*) 0)))

(defun compute-current-scores (seconds)
  (loop :for reindeer :being :each :hash-value :of *reindeers*
        :collect (list (distance-travelled-secs reindeer seconds) (name reindeer))))

(defun find-winners (current-scores)
  (let* ((sorted-scores (sort current-scores #'> :key #'car))
         (high-score (caar sorted-scores)))
    (remove-if-not #'(lambda (x) (= x high-score)) sorted-scores :key #'car)))

(defun tally-winners (winners)
  (loop :for w :in winners
        :do (let ((score (gethash (cadr w) *scores*)))
              (setf (gethash (cadr w) *scores*) (+ score 1)))))

(defun get-best-score ()
  (loop :with best-score = 0
        :for score :being :each :hash-value :of *scores*
        :do (if (> score best-score)
                (setf best-score score))
        :finally (return best-score)))

(defun distance-travelled-secs (reindeer seconds)
  (with-slots ((speed-of-move rate) (duration-of-move duration) (duration-of-rest rest-time)) reindeer
    (let* ((duration-of-cycle         (+ duration-of-move duration-of-rest))
           (distance-per-cycle        (* speed-of-move duration-of-move))
           (number-of-cycles          (floor (/ seconds duration-of-cycle)))
           (remainder                 (rem seconds duration-of-cycle))
           (completed-cycles-distance (* number-of-cycles distance-per-cycle)))

      (if (< remainder duration-of-move)
          (+ completed-cycles-distance (* remainder speed-of-move))
          (+ completed-cycles-distance distance-per-cycle)))))

(defun find-part2-winner (data seconds)
  (populate-reindeers data)
  (init-scores-table data)
    
  (loop :for s :from 1 :upto seconds
        :do (let* ((current-scores (compute-current-scores s))
                   (winners        (find-winners current-scores)))
              (tally-winners winners))
        :finally (return (get-best-score))))
