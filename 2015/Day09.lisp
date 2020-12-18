;;;;
;;;; Advent of Code 2015
;;;   Day 9


;; Pick a direct city-to-city connection (e.g. AlphaCentauri to Snowdin).
;; Note the distance (66).
;; Look at all other direct routes from city1 to the other cities and
;; consider only those routes that are less than 66.  If none are less
;; than 66, then note that the fastest route
;; For each dest city on those routes, 

(defparameter *input-data* (make-hash-table :test 'equal))
(defparameter *towns* (make-hash-table :test 'equal))

(defun load-data (file)
  (with-open-file (f file)
    (loop :for orig  = (read f nil nil)
	  :while orig
	  :for skip1 = (read f nil nil)
	  :for dest  = (read f nil nil)
	  :for skip2 = (read f nil nil)
	  :for dist  = (read f nil nil)
	  :collect orig :into origs
	  :collect dest :into dests
	  :collect dist :into dists
	  :finally (mapcan #'(lambda (orig dest dist)
			       (setf (gethash (list orig dest) *input-data*) dist)
			       (setf (gethash (list dest orig) *input-data*) dist)
			       (setf (gethash orig *towns*) nil)
			       (setf (gethash dest *towns*) nil))
			   origs dests dists))))

(defun get-all-towns ()
  (loop :for k :being :the :hash-keys :in *towns*
	:collect k))

;; permutations
;;
;;  all permutations of a list L is:
;;    for each element E in L:
;;       that element prepended to all permutations of [ L with E removed ]

(defun all-permutations (list)
  (cond ((null (cdr list)) (list list))
	(t (loop :for element :in list
		 :append (mapcar (lambda (l)
				   (cons element l))
				 (all-permutations (remove element list)))))))

(defun distance (lst &optional (sum 0))
  (cond ((null (cdr lst)) sum)
	(t (distance
	    (cdr lst)
	    (+ (gethash (list (car lst) (cadr lst)) *input-data*)
	       sum)))))

(defun shortest-path ()
  (do* ((routes (all-permutations (get-all-towns)) (cdr routes))
	(current-route (car routes) (car routes))
	(current-distance (distance current-route) (distance current-route))
	(shortest current-distance)
	(best-route current-route))
       ((null current-route) (format t "best route = ~s, distance = ~d"
				     best-route shortest))
    (if (< current-distance shortest)
	(progn
	  (setf shortest current-distance)
	  (setf best-route current-route)))))

(defun longest-path ()
  (do* ((routes (all-permutations (get-all-towns)) (cdr routes))
	(current-route (car routes) (car routes))
	(current-distance (distance current-route) (distance current-route))
	(longest current-distance)
	(best-route current-route))
       ((null current-route) (format t "best route = ~s, distance = ~d"
				     best-route longest))
    (if (> current-distance longest)
	(progn
	  (setf longest current-distance)
	  (setf best-route current-route)))))
