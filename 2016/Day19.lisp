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

;;; ***** Part 2 *****

;;; I'm not finding a mathematical way to solve part 2.  For each elf (of n remaining elves) whose turn it is, we have to eliminate
;;; the elf that is (floor (/ n 2)) away from that elf.  In other words, the elf "across the circle" from that elf.  So, it will have
;;; to bel solved via loops and structures.

(defparameter *elves* (make-array *input* :element-type 'cons))

(defun init-elves (&optional (num-elves *input*))
  (let ((*input* num-elves))
    (setf *elves* (make-array num-elves :element-type 'cons))
    (loop :for i :below num-elves
          :do (let ((p  (if (= i 0) (1- num-elves) (1- i)))
                    (n  (if (= i (1- num-elves)) 0 (1+ i))))
                (setf (elt *elves* i) (cons p n))))))

(defun next-elf (elf)
  (cdr (elt *elves* elf)))

(defun delete-elf (elf)
  (let* ((elf-data (elt *elves* elf))
         (prev  (car elf-data))
         (next  (cdr elf-data)))
    
    (setf (cdr (elt *elves* prev)) next
          (car (elt *elves* next)) prev)))

(defun test-part2 (elves)
  (init-elves elves)
  (find-elf-part2))

(defun find-elf-part2 ()
  (loop :with num-elves = (length *elves*)
        :with start = 0
        :with mid   = (floor (/ num-elves 2))
        :for i :below num-elves
        :do (progn
              (delete-elf mid)
              (setf mid (next-elf mid))
              (if (oddp (- num-elves i))
                  (setf mid (next-elf mid)))
              (setf start (next-elf start)))
        :finally (return (1+ start))))

(defun part2 ()
  (init-elves *input*)
  (find-elf-part2))
