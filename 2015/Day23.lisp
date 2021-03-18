;;;;
;;;; Advent of Code 2015
;;;;   Day 22

(defpackage #:Day23
  (:use #:cl))

(in-package #:Day23)

(defparameter *registers* '((a . 0) (b . 0)))
(defparameter *instructions* (make-array 10
                                         :initial-element nil
                                         :fill-pointer 0
                                         :adjustable t))

(defparameter *input-file* "~/quicklisp/local-projects/rich/advent/2015/Day23.txt")

(defun hlf (r)
  (setf (cdr (assoc r *registers*)) (/ (cdr (assoc r *registers*)) 2)))

(defun tpl (r)
  (setf (cdr (assoc r *registers*)) (* (cdr (assoc r *registers*)) 3)))

(defun inc (r)
  (incf (cdr (assoc r *registers*))))

(defun parse-input (line)
  (let* ((first-space (search " " line))
         (comma       (search ", " line))
         (command     (subseq line 0 first-space))
         (register    (if comma
                          (subseq line (1+ first-space) comma)
                          (subseq line (1+ first-space))))
         (jio-jie     (if comma
                          (parse-integer (subseq line (+ comma 2))))))
    (if (string= command "jmp")
        (list command (parse-integer register))
        (if jio-jie
            (list command register jio-jie)
            (list command register)))))

(defun save-instruction (instruction)
  (vector-push-extend instruction *instructions*))

(defun parse-and-save (line)
  (save-instruction (parse-input line)))

(defun load-instructions (file)
  (setf (fill-pointer *instructions*) 0)
  (with-open-file (stream file)
    (loop :for line = (read-line stream nil nil)
          :while line
          :do (parse-and-save line))))
