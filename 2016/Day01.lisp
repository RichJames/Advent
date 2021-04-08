;;;;
;;;;  Advent of Code 2016
;;;;     Day 01

(defpackage #:Day01
  (:use #:cl))

(in-package #:Day01)

(defparameter *test1* '(R2 L3))
(defparameter *test2* '(R2 R2 R2))
(defparameter *test3* '(R5 L5 R5 R3))

(defparameter *input* '(R4 R5 L5 L5 L3 R2 R1 R1 L5 R5 R2 L1 L3 L4 R3 L1 L1 R2 R3 R3 R1 L3 L5 R3 R1 L1 R1 R2 L1 L4 L5 R4 R2 L192 R5 L2 R53 R1 L5 R73 R5 L5 R186 L3 L2 R1 R3 L3 L3 R1 L4 L2 R3 L5 R4 R3 R1 L1 R5 R2 R1 R1 R1 R3 R2 L1 R5 R1 L5 R2 L2 L4 R3 L1 R4 L5 R4 R3 L5 L3 R4 R2 L5 L5 R2 R3 R5 R4 R2 R1 L1 L5 L2 L3 L4 L5 L4 L5 L1 R3 R4 R5 R3 L5 L4 L3 L1 L4 R2 R5 R5 R4 L2 L4 R3 R1 L2 R5 L5 R1 R1 L1 L5 L5 L2 L1 R5 R2 L4 L1 R4 R3 L3 R1 R5 L1 L4 R2 L3 R5 R3 R1 L3))

(defparameter *cur-dir* 'n)
(defparameter *cur-pos-x* 0)
(defparameter *cur-pos-y* 0)

(defun reset ()
  (setf *cur-dir*   'n
        *cur-pos-x* 0
        *cur-pos-y* 0))

(defun parse-instruction (inst)
  (let* ((str-instr (symbol-name inst))
         (direction (subseq str-instr 0 1))
         (distance (parse-integer (subseq str-instr 1))))
    (values direction distance)))

(defun update-pos (inst)
  (multiple-value-bind (dir dist) (parse-instruction inst)
    (cond ((equal *cur-dir* 'n) (setf *cur-dir* (if (equal dir "R") 'e 'w)))
          ((equal *cur-dir* 'e) (setf *cur-dir* (if (equal dir "R") 's 'n)))
          ((equal *cur-dir* 's) (setf *cur-dir* (if (equal dir "R") 'w 'e)))
          ((equal *cur-dir* 'w) (setf *cur-dir* (if (equal dir "R") 'n 's))))

    (cond ((equal *cur-dir* 'n) (setf *cur-pos-y* (+ *cur-pos-y* dist)))
          ((equal *cur-dir* 'e) (setf *cur-pos-x* (+ *cur-pos-x* dist)))
          ((equal *cur-dir* 's) (setf *cur-pos-y* (- *cur-pos-y* dist)))
          ((equal *cur-dir* 'w) (setf *cur-pos-x* (- *cur-pos-x* dist))))))

(defun follow-insts (insts)
  (reset)
  (loop :for i :in insts
        :do (update-pos i))
  (values *cur-dir* *cur-pos-x* *cur-pos-y*))

(defun part1 (insts)
  (multiple-value-bind (dir x y) (follow-insts insts)
    (format t "Facing ~a, Easter Bunny Headquarters is ~a blocks away.~%" dir (+ (abs x) (abs y)))))

;;; ****** Part 2 ********

(defparameter *visited-locations* (make-hash-table :test 'equal))

(defparameter *test4* '(R8 R4 R4 R8))

(defun reset2 ()
  (reset)
  (clrhash *visited-locations*))

(defun follow-and-record-insts (insts)
  (reset2)

  (loop :for i :in insts
        :until (apply-inst i))

  (values *cur-dir* *cur-pos-x* *cur-pos-y*))

(defun part2 (insts)
  (multiple-value-bind (dir x y) (follow-and-record-insts insts)
    (format t "Facing ~a, Easter Bunny Headquarters is ~a blocks away.~%" dir (+ (abs x) (abs y)))))


(defun apply-inst (inst)
  (multiple-value-bind (dir dist) (parse-instruction inst)
    (update-dir dir)
    (walk-distance dist)))

(defun update-dir (dir)
  (cond ((equal *cur-dir* 'n) (setf *cur-dir* (if (equal dir "R") 'e 'w)))
        ((equal *cur-dir* 'e) (setf *cur-dir* (if (equal dir "R") 's 'n)))
        ((equal *cur-dir* 's) (setf *cur-dir* (if (equal dir "R") 'w 'e)))
        ((equal *cur-dir* 'w) (setf *cur-dir* (if (equal dir "R") 'n 's)))))

(defun walk-distance (dist)
  (let ((x     (if (member *cur-dir* '(e w)) (if (equal *cur-dir* 'e) 1 -1) 0))
        (y     (if (member *cur-dir* '(n s)) (if (equal *cur-dir* 'n) 1 -1) 0)))
    (loop :for i :from 1 :upto (abs dist)
          :when (update-and-record-location x y)
            :return t)))

(defun update-and-record-location (x y)
  (setf *cur-pos-x* (+ *cur-pos-x* x))
  (setf *cur-pos-y* (+ *cur-pos-y* y))
  (let ((revisited (gethash (list *cur-pos-x* *cur-pos-y*) *visited-locations*)))
    (if (not revisited)
        (setf (gethash (list *cur-pos-x* *cur-pos-y*) *visited-locations*) t))
    revisited))

(defun get-state ()
  (format t "Facing ~a, x = ~a, y = ~a~%" *cur-dir* *cur-pos-x* *cur-pos-y*))
