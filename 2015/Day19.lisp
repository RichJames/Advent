;;;;
;;;; Advent of Code 2015
;;;;   Day 19

(defpackage #:Day19
  (:use #:cl))

(in-package #:Day19)

(defparameter *test-molecule* "HOH")
(defparameter *test-replacements* '(("H" . "HO") ("H" . "OH") ("O" . "HH")))

(defparameter *calibration-molecules* (make-hash-table :test 'equal))

(defun replace-in-string (element string position)
  (let* ((length-to-replace (length (car element)))
         (replace-with      (cdr element))
         (first             (subseq string 0 position))
         (last              (subseq string (+ position length-to-replace))))
    (concatenate 'string first replace-with last)))

(defun record-calibration (string)
  (setf (gethash string *calibration-molecules*) t))

(defun calibrate-molecule (molecule replacements)
  (loop :for element :in replacements
        :do (loop :with search-start = 0
                  :for position = (search (car element) molecule :start2 search-start)
                  :while position
                  :do (progn
                        (record-calibration (replace-in-string element molecule position))
                        (setf search-start (+ position (length (car element))))))))

(defun test-calibration ()
  (clrhash *calibration-molecules*)
  (calibrate-molecule *test-molecule* *test-replacements*))
