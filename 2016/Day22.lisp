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
