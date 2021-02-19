;;;;
;;;; Advent of Code 2015
;;;;   Day 10

(defpackage #:Day10
  (:use #:cl))

(in-package #:Day10)

(defparameter *input* "1113122113")

;; Atom data is from:
;; http://www.nathanieljohnston.com/2010/10/a-derivation-of-conways-degree-71-look-and-say-polynomial/

(defstruct ls-atom
  number
  subsequence
  length
  evolves-to)

(defparameter *atoms* (make-array 92 :fill-pointer 0 :element-type 'ls-atom))

;; The atom data is in a file called day10atoms.txt
(defun load-data (file)
  (with-open-file (myfile file)
    (loop :for nbr = (read myfile nil nil)
	  :while nbr
	  :for seq = (read myfile nil nil)
	  :for len = (read myfile nil nil)
	  :for evo = (read myfile nil nil)
	  :do (vector-push (make-ls-atom :number nbr
					 :subsequence (write-to-string seq)
					 :length len
					 :evolves-to evo)
			   *atoms*))))

(defun atom-match (str)
  (find-if #'(lambda (subseq)
	       (let ((res (search subseq str))) ; search for subseq in str
		 (and res (= res 0))))
	   *atoms*
	   :key #'ls-atom-subsequence
	   :from-end t))

(defparameter *buffer1* (make-array 50000 :fill-pointer 0 :adjustable t))
(defparameter *buffer2* (make-array 50000 :fill-pointer 0 :adjustable t))

(defparameter *results* *buffer1*)
(defparameter *prev-results* *buffer2*)

(defun reset-results ()
  (setf (fill-pointer *results*) 0))

(defun initialize-buffers ()
  (setf (fill-pointer *buffer1*) 0)
  (setf (fill-pointer *buffer2*) 0)
  (setf *results* *buffer1*)
  (setf *prev-results* *buffer2*))

(defun switch-buffers ()
  (let ((temp *prev-results*))
    (setf *prev-results* *results*)
    (setf *results* temp)
    (reset-results)))

(defun get-atom-number (input)
  (ls-atom-number (atom-match input)))

(defun save-atom (nbr)
  (vector-push-extend nbr *results*))

(defun record-atoms (lst)
  (loop :for atom :in lst
        :do (save-atom atom)))

(defun sum-atoms ()
  (reduce #'+ (map 'vector #'(lambda (atom) (ls-atom-length (aref *atoms* (1- atom)))) *results*)))

(defun look-say (input &optional (reps 1))
  (initialize-buffers)
  (save-atom (get-atom-number input))
  (loop :for i :below reps
        :do (progn
              (switch-buffers)
              (loop :for atom :across *prev-results*
                    :do (record-atoms (ls-atom-evolves-to (aref *atoms* (1-  atom))))))
        :finally (return (sum-atoms))))
