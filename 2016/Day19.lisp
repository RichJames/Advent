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

(defparameter *elves* (make-array *input* :fill-pointer 0))

(defun init-elves (&optional (num-elves *input*))
  (setf *elves* (make-array num-elves :fill-pointer 0))
  (loop :for i :below num-elves
        :do (vector-push (1+ i) *elves*)))

(defun next-elf (elf)
  (declare (integer elf))
  (declare (type (simple-array integer (*)) *elves*))
  (cond ((= elf (length *elves*)) 0)
        (t (rem (1+ elf) (length *elves*)))))

(defun eliminate-elf (&key chooser)
  (declare (integer chooser))
  (declare (type (vector) *elves*))
  (let* ((steps  (ash (length *elves*) -1))
         (chosen (rem (+ chooser steps) (length *elves*))))
    (setf *elves* (remove (aref *elves* chosen) *elves*))
    chosen))

(defun find-elf-part2 ()
  (loop :while (> (length *elves*) 1)
        :for elf = 0 :then (if (> eliminated-elf elf) (next-elf elf) (next-elf (1- elf)))
        :for eliminated-elf = (eliminate-elf :chooser elf)
        :finally (return (aref *elves* 0))))

(defun test-part2 (elves)
  (init-elves elves)
  (find-elf-part2))

(defun part2 ()
  (init-elves)
  (find-elf-part2))

(defun test-sets ()
  (format t "Elves: ~a, winner: ~a~%" 2 (test-part2 2))
  (format t "Elves: ~a, winner: ~a~%" 3 (test-part2 3))
  (format t "Elves: ~a, winner: ~a~%" 4 (test-part2 4))
  (format t "Elves: ~a, winner: ~a~%" 5 (test-part2 5))
  (format t "Elves: ~a, winner: ~a~%" 6 (test-part2 6))
  (format t "Elves: ~a, winner: ~a~%" 7 (test-part2 7))
  (format t "Elves: ~a, winner: ~a~%" 8 (test-part2 8))
  (format t "Elves: ~a, winner: ~a~%" 9 (test-part2 9))
  (format t "Elves: ~a, winner: ~a~%" 10 (test-part2 10)))

;;; *** Class-based approach, using next and previous indexes

(defclass elf ()
  ((prev :initarg :prev :initform (error "Must specify value for prev") :accessor prev :type fixnum)
   (next :initarg :next :initform (error "Must specify value for next") :accessor next :type fixnum)))

(defparameter *elves* (make-array *input* :element-type 'elf))

(defun init-elves (&optional (num-elves *input*))
  (let ((*input* num-elves))
    (setf *elves* (make-array num-elves :fill-pointer 0))
    (loop :repeat num-elves
          :do (vector-push (make-instance 'elf) *elves*))))

(defun init-elves (&optional (num-elves *input*))
  (let ((*input* num-elves))
    (setf *elves* (make-array num-elves :element-type 'elf))
    (loop :for i :below num-elves
          :do (let ((p  (if (= i 0)
                            (1- num-elves)
                            (1- i)))
                    (n  (if (= i (1- num-elves))
                            0
                            (1+ i))))
                (setf (elt *elves* i) (make-instance 'elf :prev p :next n))))))

(defun next-elf (elf)
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array elf) *elves*))
  (the integer (slot-value (elt *elves* elf) 'next)))

(defun eliminate-elf (&key chooser steps)
  (declare (optimize (speed 3) (safety 0))
           (fixnum chooser steps)
           (type (simple-array elf) *elves*))
  
  (loop :repeat steps
        :for chosen-elf = (next-elf chooser) :then (next-elf chosen-elf)
        :finally (with-slots (prev next) (elt *elves* chosen-elf)
                   (setf (slot-value (elt *elves* prev) 'next) next
                         (slot-value (elt *elves* next) 'prev) prev))))

(defun find-elf-part2 ()
  (loop :repeat (1- (length *elves*))
        :for remaining-elves :downfrom (length *elves*)
        :for steps = (ash remaining-elves -1)
        :for elf = 0 :then (next-elf elf)
        :do (eliminate-elf :chooser elf :steps steps)
        :finally (return (1+ elf))))

(defparameter *a* (make-array 5 :element-type 'integer :initial-element 1))

(defun foo ()
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array * (*)) *a*))
  (elt *a* 0))


(defun foo ()
  (declare (optimize (speed 3) (safety 0)))
  (elt *a* 0))

(defun foo ()
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array integer) *a*))
  (the integer (elt *a* 0)))
