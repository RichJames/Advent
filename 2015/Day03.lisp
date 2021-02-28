;;;;
;;;; Advent of Code 2015
;;;;   Day 03

(defpackage #:Day03
  (:use #:cl))

(in-package #:Day03)

;; Delivering packages to houses

;; Use hash table to record houses visited.
;; Houses are identified by their x,y coordinate.

(defparameter *houses* (make-hash-table :test 'equal))

(defun delivery-packages (file)
  (clrhash *houses*)
  (setf (gethash '(0 0) *houses*) t)
  (with-open-file (myfile file :direction :input)
    (loop :with sx := 0 :with sy := 0
	  :with rx := 0 :with ry := 0
	  :with odd := t
	  :for dir := (read-char myfile nil nil)
	  :while dir
	  :do (progn
		(cond ((equal dir #\>) (incf x))
		      ((equal dir #\<) (decf x))
		      ((equal dir #\^) (incf y))
		      ((equal dir #\v) (decf y)))
		(setf (gethash (list x y) *houses*) t))))
  (loop :for k :being :the :hash-keys :in *houses* :using (hash-value v)
	:count v))

;;; Need to treat Santa's position as a 2-element list. E.g. starting
;;; position will be '(0 0).

(defun get-move (dir)
  (cond ((equal dir #\>) '( 1  0))
	((equal dir #\<) '(-1  0))
	((equal dir #\^) '( 0  1))
	((equal dir #\v) '( 0 -1))
	(t (format t "~%Unknown dir command: ~s" dir))))

(defun apply-move (pos move)
  (mapcar #'+ pos move))

(defun deliver-packages (file)
  (clrhash *houses*)
  (setf (gethash '(0 0) *houses*) t)
  (let ((santa '(0 0))
	(robot '(0 0))
	(santa-moving nil))
    
    (with-open-file (myfile file :direction :input)
      (loop :for dir := (read-char myfile nil nil)
	    :while (member dir '(#\> #\< #\^ #\v))
	    :do (progn
		  (setf santa-moving (not santa-moving))
		  (if santa-moving
		      (progn
			(setf santa (apply-move santa (get-move dir)))
			(setf (gethash santa *houses*) t))
		      (progn
			(setf robot (apply-move robot (get-move dir)))
			(setf (gethash robot *houses*) t)))))))
  
  (loop :for k :being :the :hash-keys :in *houses* :using (hash-value v)
	:count v))

;; Redo above using do loop instead of loop.
(defun deliver-packages (file)
  (clrhash *houses*)
  (setf (gethash '(0 0) *houses*) t)

  (with-open-file (myfile file :direction :input)
    (do* ((dir (read-char myfile nil nil) (read-char myfile nil nil))
	  (move (get-move dir) (get-move dir))
	  (valid-commands '(#\> #\< #\^ #\v))
	  (santa '(0 0))
	  (robot '(0 0))
	  (santa-moving t (not santa-moving)))
	 ((not (member dir valid-commands)))
      (cond (santa-moving
	     (setf santa (apply-move santa move))
	     (setf (gethash santa *houses*) t))
	    ((not santa-moving)
	     (setf robot (apply-move robot move))
	     (setf (gethash robot *houses*) t)))))

  (loop :for k :being :the :hash-keys :in *houses* :using (hash-value v)
	:count v))

(defun valid-command-p (cmd)
  (let ((valid-commands '(#\> #\< #\^ #\v)))
    (member cmd valid-commands)))

