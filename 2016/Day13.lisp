;;;;
;;;;  Advent of Code 2016
;;;;     Day 13

(defpackage #:2016Day13
  (:use #:cl))

(in-package #:2016Day13)

(defparameter *favorite-number* 1358)
(defparameter *starting-position* '(1 1))
(defparameter *target* '(31 39))

(defun is-wall-p (x y)
  (let* ((number (+ (* x x) (* x 3) (* 2 x y) y (* y y) *favorite-number*))
         (num-bits (logcount number)))
    (oddp num-bits)))

(defun is-space-p (x y)
  (not (is-wall-p x y)))

(defun print-maze (cols rows)
  (format t "  ")
  (loop :for c :below cols
        :do (format t "~a" c)
        :finally (format t "~%"))
  (loop :for r :below rows
        :do (progn
              (format t "~a " r)
              (loop :for c :below cols
                    :do (format t "~a" (if (is-wall-p c r) "#" "."))
                    :finally (format t "~%")))))

;;; ***** Breadth-first search *****

;;; **** queue implementations from Common Lisp Recipes ****
(defclass queue ()
  ((list :initform nil)
   (tail :initform nil)))

(defmethod dequeue ((queue queue))
  (with-slots (list) queue
    (pop list)))

(defmethod enqueue (new-item (queue queue))
  (with-slots (list tail) queue
    (let ((new-tail (list new-item)))
      (cond ((null list) (setf list new-tail))
            (t (setf (cdr tail) new-tail)))
      (setf tail new-tail))))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t)
    (with-slots (list tail) queue
      (cond ((cddddr list)
             ;; at least five elements, so print ellipsis
             (format stream "(~{~S ~}... ~S)" (subseq list 0 3) (first tail)))
            ;; otherwise, print whole list
            (t (format stream "~:S" list))))))

(defparameter *queue* (make-instance 'queue))

;;; **** end of queue implementation ****

(defclass node-info ()
  ((distance :initarg :distance :initform 0)
   (prev-node :initarg :prev-node :initform nil)))

(defparameter *nodes* (make-hash-table :test 'equal))

(defun record-space (&key from to)
  (if (not (gethash to *nodes*))
      (let* ((prev-info  (gethash from *nodes*))
             (prev-dist  (slot-value prev-info 'distance)))
        
        (enqueue to *queue*)
        (setf (gethash to *nodes*) (make-instance 'node-info
                                                  :distance (1+ prev-dist)
                                                  :prev-node from)))))

(defun get-neighbor-nodes (from)
  (let ((right  (if (> (first from) 0) (list (1- (first from)) (second from))))
        (left   (list (1+ (first from)) (second from)))
        (up     (if (> (second from) 0)  (list (first from) (1- (second from)))))
        (down   (list (first from) (1+ (second from)))))

    (remove-if #'(lambda (x) (is-wall-p (first x) (second x))) (remove-if #'null (list right left up down)))))

(defun process-queue ()
  (flet ((report-end (node)
           (format t "Shortest distance to ~a, starting from ~a, is ~a."
                   node
                   *starting-position*
                   (slot-value (gethash node *nodes*) 'distance))))

    (loop :named find-end
          :for node = (dequeue *queue*)
          :while node
          :for neighbors = (get-neighbor-nodes node)
          :do (loop :for neighbor :in neighbors
                    :do (record-space :from node :to neighbor)
                    :if (equal node *target*)
                      :do (return-from find-end (report-end node))))))

(defun reset ()
  (clrhash *nodes*)
  (setf (gethash *starting-position* *nodes*) (make-instance 'node-info)
        *queue*                               (make-instance 'queue))
  (enqueue *starting-position* *queue*))

(defun test-part1 ()
  (reset)
  (setf *favorite-number* 10
        *target*          '(7 4))
  (process-queue))

(defun part1 ()
  (reset)
  (setf *favorite-number* 1358
        *target*          '(31 39))
  (process-queue))

;;; ***** Part 2 *****

(defun get-neighbor-nodes-part2 (from)
  (if (< (slot-value (gethash from *nodes*) 'distance) 50)
      (let ((right  (if (> (first from) 0) (list (1- (first from)) (second from))))
            (left   (list (1+ (first from)) (second from)))
            (up     (if (> (second from) 0)  (list (first from) (1- (second from)))))
            (down   (list (first from) (1+ (second from)))))

        (remove-if #'(lambda (x) (is-wall-p (first x) (second x))) (remove-if #'null (list right left up down))))))

(defun process-queue-part2 ()
  (loop :for node = (dequeue *queue*)
        :while node
        :for neighbors = (get-neighbor-nodes-part2 node)
        :do (loop :for neighbor :in neighbors
                  :do (record-space :from node :to neighbor))
        :finally (format t "Numbers of spaces within 50 steps: ~a~%" (hash-table-count *nodes*))))

(defun part2 ()
  (reset)
  (setf *favorite-number* 1358)
  (process-queue-part2))
