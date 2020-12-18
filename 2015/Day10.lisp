;;;;
;;;; Advent of Code 2015
;;;   Day 10

(defparameter *input* "1113122113")

(defun look-say (input)
  (declare (optimize (speed 3) (safety 0)))
  (loop	:with prev-c = nil
	:with c-count fixnum  = 0
	:with result = ""
	:for c :across input
	:when (and (not (equal c prev-c))
		   (not (null prev-c)))
	  :do (progn
		(setf result (concatenate 'string
					  result
					  (write-to-string c-count)
					  (format nil "~a" prev-c)))
		(setf prev-c nil)
		(setf c-count 0))
	:do (progn
	      (incf c-count)
	      (setf prev-c c))
	:finally (return (concatenate 'string
				      result
				      (write-to-string c-count)
				      (format nil "~a" prev-c)))))

(defun part1 (input &optional (reps 40))
  (loop :for i :below reps
	:for result = (look-say input) :then (look-say result)
	:finally (return result)))
