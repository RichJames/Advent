;;;;
;;;;  Advent of Code 2016
;;;;     Day 12

(defpackage #:2016Day12
  (:use #:cl #:cl-ppcre))

(in-package #:2016Day12)


(defparameter *registers* '((a . 0) (b . 0) (c . 0) (d . 0)))

(defparameter *instructions* (make-array 1 :fill-pointer 0 :adjustable t))
(defparameter *instruction-pointer* 0)

(defun reset ()
  (setf (fill-pointer *instructions*) 0
        *instruction-pointer*         0
        (cdr (assoc 'a *registers*))  0
        (cdr (assoc 'b *registers*))  0
        (cdr (assoc 'c *registers*))  0
        (cdr (assoc 'd *registers*))  0))

(defun cpy (x y)
  (cond ((numberp x) (setf (cdr (assoc y *registers*)) x))
        (t           (setf (cdr (assoc y *registers*)) (cdr (assoc x *registers*)))))

  (incf *instruction-pointer*))

(defun inc (x)
  (incf (cdr (assoc x *registers*)))
  (incf *instruction-pointer*))

(defun dec (x)
  (decf (cdr (assoc x *registers*)))
  (incf *instruction-pointer*))

(defun jnz (x y)
  (let ((x-val  (if (symbolp x) (cdr (assoc x *registers*)) x)))
    (if (not (zerop x-val))
        (incf *instruction-pointer* y)
        (incf *instruction-pointer*))))

(defun execute-instruction (instr)
  (case (first instr)
    ((cpy) (cpy (second instr) (third instr)))
    ((jnz) (jnz (second instr) (third instr)))
    ((inc) (inc (second instr)))
    ((dec) (dec (second instr)))))

(defun load-instructions ()
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2016/Day12.txt")
    (loop :for instruction-string = (read-line stream nil nil)
          :while instruction-string
          :do (loop :for item :in (ppcre:split "\\s+" instruction-string)
                    :collect (let ((item-as-number (parse-integer item :junk-allowed t)))
                               (if (numberp item-as-number)
                                   item-as-number
                                   (intern (string-upcase item)))) :into instr-list
                    :finally (vector-push-extend instr-list *instructions*)))))

(defun process-instructions ()
  (loop :while (< *instruction-pointer* (fill-pointer *instructions*))
        :for instruction = (aref *instructions* *instruction-pointer*)
        :do (execute-instruction instruction)
        :finally (return (cdr (assoc 'a *registers*)))))

(defun part1 ()
  (reset)
  (load-instructions)
  (process-instructions))
