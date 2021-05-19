;;;;
;;;;  Advent of Code 2017
;;;;     Day 17

(defpackage #:2016Day17
  (:use #:cl))

(in-package #:2016Day17)

(defparameter *input* ".^^.^^^..^.^..^.^^.^^^^.^^.^^...^..^...^^^..^^...^..^^^^^^..^.^^^..^.^^^^.^^^.^...^^^.^^.^^^.^.^^.^.")
(defparameter *num-rows* 40)

(defparameter *map* (make-array (list *num-rows* (length *input*))))

(defun initialize-map (input)
  (let ((input input))
    (loop :for i :below (array-dimension *map* 1)
          :for c :across input
          :do (setf (aref *map* 0 i) c))))

(defun get-tile (prev-tiles)
  (if (or (equal prev-tiles '(#\^ #\^ #\.))
          (equal prev-tiles '(#\. #\^ #\^))
          (equal prev-tiles '(#\^ #\. #\.))
          (equal prev-tiles '(#\. #\. #\^)))
      #\^
      #\.))

(defun get-prev-tiles (row col)
  (cond ((= row 0) (list #\. #\. #\.))
        ((= col 0) (list #\.
                         (aref *map* (1- row) col)
                         (aref *map* (1- row) (1+ col))))
        ((= col (1- (length *input*))) (list (aref *map* (1- row) (1- col))
                                             (aref *map* (1- row) col)
                                             #\.))
        (t (list (aref *map* (1- row) (1- col))
                 (aref *map* (1- row) col)
                 (aref *map* (1- row) (1+ col))))))

(defun fill-map ()
  (destructuring-bind (rows cols) (array-dimensions *map*)
    (loop :for r :from 1 :below rows
          :do (loop :for c :below cols
                    :do (setf (aref *map* r c) (get-tile (get-prev-tiles r c)))))))

(defun print-map ()
  (loop :for r :below *num-rows*
        :for offset = 0 :then (incf offset 100)
        :do (format t "~{~a~}~%" (coerce (make-array 100 :displaced-to *map* :displaced-index-offset offset) 'list))))

(defun part1 ()
  (setf *num-rows* 40
        *map*      (make-array (list *num-rows* (length *input*))))
  (initialize-map *input*)
  (fill-map)

  (destructuring-bind (rows cols) (array-dimensions *map*)
    (count #\. (make-array (* rows cols) :displaced-to *map* :displaced-index-offset 0))))

;;; ***** Part 2 *****

(defun part2 ()
  (setf *num-rows* 400000
        *map*      (make-array (list *num-rows* (length *input*))))
  (initialize-map *input*)
  (fill-map)

  (destructuring-bind (rows cols) (array-dimensions *map*)
    (count #\. (make-array (* rows cols) :displaced-to *map* :displaced-index-offset 0))))
