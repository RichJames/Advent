;;;;
;;;;  Advent of Code 2017
;;;;     Day 17

(defpackage #:2016Day17
  (:use #:cl))

(in-package #:2016Day17)

(require "sb-md5")

(defparameter *input* "vwbaicqe")
(defparameter *position* '(0 0))
(defparameter *vault*    '(3 3))

(defun md5-calc (string)
  (let* ((md5-digest          (sb-md5:md5sum-string string))
         (md5-digest-list     (map 'list #'identity md5-digest)))
    (format nil "~(~{~2,'0x~}~)" md5-digest-list)))

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

;;; Each node in this solution represents a position in the 4x4 matrix of rooms at a point in time.
;;; A position is the combination of the coordinates of the room (0,0) - (3,3) plus the steps taken to get
;;; there.  The initial node is at position (0,0) and its ID is the input key.

(defclass node-info ()
  ((prev-node :initarg :prev-node :initform nil)
   (position :initarg :position :initform '(0 0))))

(defparameter *nodes* (make-hash-table :test 'equal))

(defun reset ()
  (clrhash *nodes*)
  (setf (gethash *input* *nodes*) (make-instance 'node-info)
        *queue*                   (make-instance 'queue))
  (enqueue *input* *queue*))

(defun get-open-doors (node)
  (let* ((hash       (md5-calc node))
         (position   (slot-value (gethash node *nodes*) 'position))
         (open-doors (loop :for i :below 4
                           :if (member (elt hash i) '(#\b #\c #\d #\e #\f))
                             :collect t :into doors
                           :else
                             :collect nil :into doors
                           :finally (return doors))))

    (remove-if #'null
               (list
                (if (and (first open-doors)  (> (second position) 0)) "U")
                (if (and (second open-doors) (< (second position) 3)) "D")
                (if (and (third open-doors)  (> (first  position) 0)) "L")
                (if (and (fourth open-doors) (< (first  position) 3)) "R")))))

(defun record-node (&key from to)
  (let* ((prev-position (slot-value (gethash from *nodes*) 'position))
         (new-node       (concatenate 'string from to))
         (new-position   (cond ((string= to "U") (list (first prev-position) (1- (second prev-position))))
                               ((string= to "D") (list (first prev-position) (1+ (second prev-position))))
                               ((string= to "L") (list (1- (first prev-position)) (second prev-position)))
                               ((string= to "R") (list (1+ (first prev-position)) (second prev-position))))))

    (enqueue new-node *queue*)
    (setf (gethash new-node *nodes*) (make-instance 'node-info
                                                    :prev-node from
                                                    :position new-position))))

(defun at-vault-p (node)
  (let* ((node-position  (slot-value (gethash node *nodes*) 'position)))
    (equal node-position *vault*)))

(defun process-queue ()
  (flet ((report-end (node)
           (format t "Shortest path to vault is ~a~%" (subseq node (length *input*)))))
    (loop :named find-vault
          :for node = (dequeue *queue*)
          :while node
          :for doors = (get-open-doors node)
          :do (loop :for door :in doors
                    :do (record-node :from node :to door)
                    :if (at-vault-p node)
                      :do (return-from find-vault (report-end node))))))

(defun part1 ()
  (reset)
  (process-queue))

;;; ***** Part 2 *****

(defun process-queue-part2 ()
  (loop :for node = (dequeue *queue*)
        :while node
        :if (at-vault-p node)
          :collect (subseq node (length *input*)) :into paths-to-vault
        :else
          :do (loop :for door :in (get-open-doors node)
                    :do (record-node :from node :to door))
        :finally (return (length (car (last paths-to-vault))))))

(defun part2 ()
  (reset)
  (process-queue-part2))


