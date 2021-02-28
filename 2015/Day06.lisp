;;;;
;;;; Advent of Code 2015
;;;   Day 6

;; Given:
;;  - a 1000 x 1000 grid of lights
;;  - all lights start turned off
;;  - coordinate pairs are inclusive (e.g. 0,0 through 2,2 refers to 9 lights in a 3x3 grid)
;;  - coordinate pairs are described as "#,# through #,#"
;;  - commands will be one of:
;;      - turn on
;;      - turn off
;;      - toggle (toggle means turn on if off or off if on)
;;
;;  - instructions will have the form: command #,# through #,#

;; We need to process the instructions in the file.
;; Then count the lights that are on.

(defparameter *row-size* 1000)
(defparameter *lights* (make-array (* *row-size* *row-size*) :initial-element 0))

(defun change-light (command row col)
  (let ((pos (+ (* row *row-size*) col)))
    (cond ((equal command "TURNON") (incf (elt *lights* pos)))
	  ((equal command "TURNOFF") (if (> (elt *lights* pos) 0) (decf (elt *lights* pos))))
	  ((equal command "TOGGLE") (setf (elt *lights* pos) (+ 2 (elt *lights* pos)))))))

(defun execute-command (command row1 col1 row2 col2)
  (loop :for i :from row1 :upto row2 :do
    (loop :for j :from col1 :upto col2 :do
	  (change-light command i j))))

(defun process-cmd-file (file)
  (with-open-file (myfile file)
    (loop :for line = (read-line myfile nil nil)
	  :while line
	  :do (progn
		(let* ((command (get-command line))
		       (coords (get-coords line))
		       (start (car coords))
		       (end (cadr coords)))
		  (execute-command command
				   (car start) (cadr start)
				   (car end) (cadr end)))))))

(defun get-command (string)
  (with-input-from-string (s string)
    (let ((c1 (read s nil nil)))
      (cond ((eq c1 'turn) (concatenate 'string (string c1) (string (read-from-string string nil nil :start 5))))
	    ((eq c1 'toggle) (string c1))
	    (t (format t "~%Unknown command: ~s" c1))))))

(defun get-coords (string)
  (let ((start (1+ (position #\space string)))
	(ws-count (count #\space string)))
    (progn
      (if (= ws-count 4)
	  (setf start (position #\space string :start start)))

      (let* ((comma1 (position #\, string :start start))
	     (ws (position #\space string :start comma1))
	     (through (search "through" string))
	     (comma2 (position #\, string :start through)))
	(let ((n1 (read-from-string string t nil :start start :end comma1))
	      (n2 (read-from-string string t nil :start (1+ comma1) :end ws))
	      (n3 (read-from-string string t nil :start (+ through 8) :end comma2))
	      (n4 (read-from-string string t nil :start (1+ comma2))))
	  (list (list n1 n2) (list n3 n4)))))))

(defun count-lights ()
  (count t *lights*))

(defun total-brightness ()
  (reduce #'+ *lights*))
