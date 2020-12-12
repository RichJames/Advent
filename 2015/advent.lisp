;;;; Advent of Code

(defun which-floor (commands)
	   (- (count #\( commands)
	      (count #\) commands)))

(defun basement (commands)
  (let ((floor 0)
	(pos 0))
    (loop :for c :in (coerce commands 'list)
	  :do (progn
		(cond ((equal c #\() (incf floor))
		      ((equal c #\)) (decf floor)))
		(incf pos))
	  :when (= floor -1)
	    :return pos
	  :finally (format t "~%floor = ~s" floor))))

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
