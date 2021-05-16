;;;;
;;;;  Advent of Code 2016
;;;;     Day 16

(defpackage #:2016Day16
  (:use #:cl))

(in-package #:2016Day16)

(defparameter *input* "10111100110001111")

(defun dragon (input)
  (let* ((b (reverse input))
         (b-new (map 'string #'(lambda (c)
                                 (cond
                                   ((equal c #\1) #\0)
                                   ((equal c #\0) #\1)
                                   (t c))) b)))
    (format nil "~a0~b" input b-new)))

(defun get-data (input quantity)
  (loop :for data = input :then (dragon data)
        :while (< (length data) quantity)
        :finally (return data)))

(defun compute-checksum (data)
  (loop :for i :from 0 :below (length data) :by 2
        :for first-char = (elt data i)
        :for second-char = (elt data (1+ i))
        :if (equal first-char second-char)
          :collect #\1 :into checksum
        :else
          :collect #\0 :into checksum
        :end
        :finally (return (concatenate 'string checksum))))

(defun find-checksum (input quantity)
  (let* ((data       (get-data input quantity))
         (data-trunc (subseq data 0 quantity)))
    (loop :for checksum = (compute-checksum data-trunc) :then (compute-checksum checksum)
          :while (evenp (length checksum))
          :finally (return checksum))))

(defun part1 ()
  (find-checksum *input* 272))

;;; ***** Part 2 *****

(defun part2 ()
  (find-checksum *input* 35651584))
