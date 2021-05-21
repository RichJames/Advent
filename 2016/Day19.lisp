;;;;
;;;;  Advent of Code 2016
;;;;     Day 19

(defpackage #:2016Day19
  (:use #:cl))

(in-package #:2016Day19)

;;; For every set of elves that equals an even power of 2, the elf whose turn it is will win.  Thus, if/when you reduce a set
;;; of elves to a set that is equal to a power of 2, the elf whose turn it is will win in that set.  So, if your set is
;;; greater than the largest power of 2 less than the set, the difference is how many elves need to be eliminated.  Count
;;; forward 2 * (eliminated elves) to get the winner.

(defparameter *input* 3004953)

(defun msb (n)
  (let ((shift (floor (log n 2))))
    (ash 1 shift)))

(defun find-elf (num-elves)
  (let ((num-to-skip (logxor num-elves (msb num-elves))))
    (+ 1 (* 2 num-to-skip))))

(defun part1 ()
  (find-elf *input*))
