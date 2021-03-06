;;;;
;;;; Advent of Code 2015
;;;;   Day 19

(defpackage #:Day19
  (:use #:cl))

(in-package #:Day19)

(defparameter *calibration-molecules* (make-hash-table :test 'equal))

(defparameter *test-molecule* "HOH")
(defparameter *test-replacements* '(("H" . "HO") ("H" . "OH") ("O" . "HH")))

(defparameter *molecule* nil)
(defparameter *replacements* nil)

(defparameter *input-data* "~/quicklisp/local-projects/rich/advent/2015/Day19.txt")

(defun load-replacement (string)
  (let* ((delimiter    " => ")
         (delim-start  (search delimiter string))
         (to-find      (subseq string 0 delim-start))
         (replace-with (subseq string (+ delim-start (length delimiter)))))
    (setf *replacements* (append *replacements* (cons (cons to-find replace-with) nil)))))

(defun load-input (file)
  (setf *molecule* nil)
  (setf *replacements* nil)
  (with-open-file (stream file)
    (loop :for line = (read-line stream nil nil)
          :while line
          :if (equal line "")
            :do (setf *molecule* (read-line stream nil nil))
          :else
            :do (load-replacement line))))

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
  (calibrate-molecule *test-molecule* *test-replacements*)
  (hash-table-count *calibration-molecules*))

(defun calibrate ()
  (clrhash *calibration-molecules*)
  (load-input *input-data*)
  (calibrate-molecule *molecule* *replacements*)
  (hash-table-count *calibration-molecules*))
