;;;;
;;;;  Advent of Code 2016
;;;;     Day 22

(defpackage #:2016Day22
  (:use #:cl :cl-ppcre))

(in-package #:2016Day22)

(defun parse-node-usage (node-df)
  (map 'list #'(lambda (x) (parse-integer x :junk-allowed t))
       (second (multiple-value-list (ppcre:scan-to-strings
                                     "node-x([0-9]+)-y([0-9]+)[\\s]+([0-9]+)T[\\s]+([0-9]+)T[\\s]+([0-9]+)T[\\s]+([0-9]+)%"
                                     node-df)))))

(defun load-disk-usage (file)
  (with-open-file (stream file)

    (loop :named outer
          :with largest-x = 0 :and largest-y = 0
          :for node-df = (parse-node-usage (read-line stream nil nil))
          :while node-df
          :do (progn
                (if (> (first node-df) largest-x) (setf largest-x (first node-df)))
                (if (> (second node-df) largest-y) (setf largest-y (second node-df))))
          :collect node-df :into df-output
          :finally (let ((node-array (make-array (list (1+ largest-x) (1+ largest-y)))))
                     (loop :for node :in df-output
                           :do (setf (aref node-array (first node) (second node)) (cddr node))
                           :finally (return-from outer node-array))))))

(defun count-viables (node-array)
  (loop :with flat-node-array = (make-array (array-total-size node-array) :displaced-to node-array)
        :with viable-nodes = 0
        :for i :below (car (array-dimensions flat-node-array))
        :for node-i = (aref flat-node-array i)
        :do (loop :for j :below (car (array-dimensions flat-node-array))
                  :for node-j = (aref flat-node-array j)
                  :if (and (/= i j)
                           (> (second node-i) 0)
                           (<= (second node-i) (third node-j)))
                    :do (incf viable-nodes))
        :finally (return viable-nodes)))

(defun part1 ()
  (count-viables (load-disk-usage "/home/rich/quicklisp/local-projects/rich/advent/2016/Day22.txt")))

;;; ***** Part 2 *****

;;; Use this to get a sense of what we are dealing with.
(defun print-grid (node-array)
  (loop :with goal-loc = (list (1- (first (array-dimensions node-array))) 0)
        :for y :below (second (array-dimensions node-array))
        :do (loop :for x :below (first (array-dimensions node-array))
                  :for node = (aref node-array x y)
                  :do (format t "~a" (cond ((and (= x (first goal-loc))
                                                 (= y (second goal-loc))) "G")
                                           ((> (second node) 86)           "#")
                                           ((= (second node) 0)           "_")
                                           (t                             "."))))
            (format t "~%")))

(defun get-neighbors (node node-array)
  "Assumes node is empty and returns neighbors whose used amounts could fit in node."
  (let ((dims (array-dimensions node-array)))
    (if (every #'< '(-1 -1) node dims)
        (let ((size (first (aref node-array (first node) (second node)))))
          (remove-if #'(lambda (x)
                         (or (notevery #'< '(-1 -1) x dims)
                             (> (second (aref node-array (first x) (second x))) size)))
                     (multiple-value-bind (x y) (values-list node)
                       (list (list (1- x) y)
                             (list (1+ x) y)
                             (list x (1- y))
                             (list x (1+ y)))))))))

;;; Need to do a BFS to find shortest path to move the empty node to the position immediately to the
;;; left of the goal node.
;;;
;;; Once that is achieved, I need a second BFS to find the shortest path to move the goal node to the
;;; (0 0) node.  Or, I can just compute the steps required to move from that starting arrangement to
;;; the target of having the goal node at position (0 0).
;;;
;;; The solution will be the sum of both BFS searches.
;;;
;;; The starting node is at (24 22).  This is the empty node.  The goal node is (31 0).  So the first
;;; BFS should figure out the shortest path to move from (24 22) to (30 0).
;;;

(defparameter *queue* (make-instance 'rju:queue))

(defclass node-info ()
  ((distance :initarg :distance :initform 0)
   (prev-node :initarg :prev-node :initform nil)))

(defparameter *nodes* (make-hash-table :test 'equal))

(defun enqueue-node (&key from to)
  (if (not (gethash to *nodes*))
      (progn
        (rju:enqueue to *queue*)
        (setf (gethash to *nodes*) (make-instance 'node-info
                                                  :distance (1+ (slot-value (gethash from *nodes*) 'distance))
                                                  :prev-node from)))))

(defun find-distance-1 (begin target node-array)
  (reset begin)
  (loop :named outer
        :for node = (rju:dequeue *queue*)
        :while node
        :for neighbors = (get-neighbors node node-array)
        :do (loop :for neighbor :in neighbors
                  :do (enqueue-node :from node :to neighbor)
                  :if (equal node target)
                    :do (return-from outer (slot-value (gethash node *nodes*) 'distance)))))

;;; This is not a general solution to this part.  It is, instead, very specific to the situation presented
;;; in this challenge.  We just need to walk the goal node left to position (0 0), which means moving it
;;; only along the x (column) dimension.  Given the empty node is to the goal node's immediate left, it takes
;;; five steps to move the goal node one position left and return the empty node to the left of the goal node.
;;; Once the goal node arrives at position (1 0) and with the empty node at position (0 0), it takes one more
;;; step to move the goal node to (0 0).
(defun find-distance-2 (node-array)
  (+ 1 (* 5 (- (first (array-dimensions node-array)) 2))))

(defun reset (starting-position)
  (clrhash *nodes*)
  (setf (gethash starting-position *nodes*) (make-instance 'node-info)
        *queue*                             (make-instance 'rju:queue))
  (rju:enqueue starting-position *queue*))

(defun part2 ()
  (let ((node-array (load-disk-usage "/home/rich/quicklisp/local-projects/rich/advent/2016/Day22.txt")))
    (+ (find-distance-1 '(24 22) '(30 0) node-array)
       (find-distance-2 node-array ))))
