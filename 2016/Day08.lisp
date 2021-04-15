;;;;
;;;;  Advent of Code 2016
;;;;     Day 08

(defpackage #:2016Day08
  (:use #:cl #:cl-ppcre))

(in-package #:2016Day08)

(defparameter *screen* (make-array '(6 50) :initial-element nil))

(defun reset ()
  (setf *screen* (make-array '(6 50) :initial-element nil)))

(defun display-screen ()
  (loop :for r :below (array-dimension *screen* 0)
        :do (loop :for c :below (array-dimension *screen* 1)
                  :do (format t "~a" (if (aref *screen* r c) "#" " "))
                  :finally (format t "~%"))))

(defun instr-rect (cols rows)
  (loop :for r :below rows
        :do (loop :for c :below cols
                  :do (setf (aref *screen* r c) t))))

(defun instr-rotate-row-once (row)
  (loop :with temp = (aref *screen* row (1- (array-dimension *screen* 1)))
        :for c :downfrom (- (array-dimension *screen* 1) 2) :to 0
        :do (setf (aref *screen* row (1+ c)) (aref *screen* row c))
        :finally (setf (aref *screen* row 0) temp)))

(defun instr-rotate-row (row amount)
  (loop :repeat amount
        :do (instr-rotate-row-once row)))

(defun instr-rotate-col-once (col)
  (loop :with temp = (aref *screen* (1- (array-dimension *screen* 0)) col)
        :for r :downfrom (- (array-dimension *screen* 0) 2) :to 0
        :do (setf (aref *screen* (1+ r) col) (aref *screen* r col))
        :finally (setf (aref *screen* 0 col) temp)))

(defun instr-rotate-col (col amount)
  (loop :repeat amount
        :do (instr-rotate-col-once col)))

(defun test ()
  (setf *screen* (make-array '(3 7) :initial-element nil))
  (instr-rect 3 2)
  (instr-rotate-col 1 1)
  (instr-rotate-row 0 4)
  (instr-rotate-col 1 1)
  (display-screen))

(defun number-of-lit-pixels ()
  (count t (make-array (array-total-size *screen*) :displaced-to *screen*)))

(defun parse-rect-instr (rect-instr)
  (let* ((x     (position #\x rect-instr))
         (space (position #\Space rect-instr))
         (col   (parse-integer (subseq rect-instr (1+ space) x)))
         (row   (parse-integer (subseq rect-instr (1+ x)))))
    (values col row)))

(defun parse-rotate-instr (rotate-instr)
  (let* ((row-instr  (search "row" rotate-instr))
         (eq-delim   (position #\= rotate-instr))
         (by-delim   (search " by " rotate-instr))
         (row-col    (parse-integer (subseq rotate-instr (1+ eq-delim) by-delim)))
         (amount     (parse-integer (subseq rotate-instr (+ by-delim 4)))))
    (values (if row-instr 'row 'col) row-col amount)))

(defun apply-instr (instr)
  (if (search "rect" instr)
      (multiple-value-bind (col row) (parse-rect-instr instr)
        (instr-rect col row))
      (multiple-value-bind (instr-type row-col amount) (parse-rotate-instr instr)
        (if (eq instr-type 'row)
            (instr-rotate-row row-col amount)
            (instr-rotate-col row-col amount)))))

(defun part1 ()
  (reset)
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2016/Day08.txt")
    (loop :for instr = (read-line stream nil nil)
          :while instr
          :do (apply-instr instr)
          :finally (progn
                     (display-screen)
                     (return (number-of-lit-pixels))))))

;;; ***** Part 2 *****

;;; Simply read the letters presented when display-screen is called.

