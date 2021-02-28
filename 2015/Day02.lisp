;;;;
;;;; Advent of Code 2015
;;;;   Day 02

(defpackage #:Day02
  (:use #:cl))

(in-package #:Day02)


(defun gift-paper (l w h)
  (let* ((s1 (* l w))
	 (s2 (* w h))
	 (s3 (* h l))
	 (extra (car (sort (list s1 s2 s3) #'<))))
    (+ (* s1 2) (* s2 2) (* s3 2) extra)))

;; Parses box dimensions of the form lxwxh and returns a list of those
;; values:
(defun get-dimensions (spec)
  (let* ((x1 (position #\x spec))
	 (x2 (position #\x spec :from-end t))
	 (l (read-from-string (subseq spec 0 x1)))
	 (w (read-from-string (subseq spec (1+ x1) x2)))
	 (h (read-from-string (subseq spec (1+ x2)))))
    (values l w h)))

;; Loop version
(defun total-gift-paper (file)
  (with-open-file (myfile file :direction :input)
    (loop :with total-paper = 0
	  :for line := (read-line myfile nil nil)
	  :while line
	  :do (multiple-value-bind (l w h) (get-dimensions line)
		(incf total-paper (gift-paper l w h)))
	  :finally (princ total-paper))))

;; Do list version
(defun total-gift-paper (file)
  (with-open-file (myfile file :direction :input)
    (do ((line (read-line myfile nil nil)
	       (read-line myfile nil nil))
	 (total-paper 0))
	((equal line nil) total-paper)
      (multiple-value-bind (l w h) (get-dimensions line)
	(incf total-paper (gift-paper l w h))))))

;;; Ribbon
;;;
;;; Need to find smallest face perimeter
;;; Need to compute bow material

(defun perimeter (x y)
  (+ (* 2 x) (* 2 y)))

(defun smallest-perimeter (l w h)
  (let ((p1 (perimeter l w))
	(p2 (perimeter w h))
	(p3 (perimeter h l)))
    (car (sort (list p1 p2 p3) #'<))))

(defun bow (l w h)
  (* l w h))

(defun package-ribbon (l w h)
  (+ (smallest-perimeter l w h) (bow l w h)))

(defun total-ribbon (file)
  (with-open-file (myfile file :direction :input)
    (loop :with tot-ribbon = 0
	  :for line := (read-line myfile nil nil)
	  :while line
	  :do (multiple-value-bind (l w h) (get-dimensions line)
		(incf tot-ribbon (package-ribbon l w h)))
	  :finally (princ tot-ribbon))))

