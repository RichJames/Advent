;;;;
;;;; Advent of Code 2015
;;;;   Day 22

(defpackage #:Day23
  (:use #:cl))

(in-package #:Day23)

;; The explanation in part 1 are NOT clear.  If followed as stated, the instructions will never end
;; as they will sit in an infinite loop on the first (aio) instruction.  The explanation says jie and
;; jio jump, but only if their respective test is met.  There is NO indication that the instruction
;; pointer moves forward 1 otherwise.  This is explicitly stated with the other, non-jumping
;; instructions.  So, I need to assume jio and jie move the pointer forward 1, unless their test
;; is met, in which case, the pointer is adjusted by the argument to the instruction.

(defparameter *registers* '(("a" . 0) ("b" . 0)))
(defparameter *instructions* (make-array 10
                                         :initial-element nil
                                         :fill-pointer 0
                                         :adjustable t))

(defparameter *inst-ptr* 0)

(defparameter *input-file* "~/quicklisp/local-projects/rich/advent/2015/Day23.txt")

(defun reset-registers ()
  (setf (cdr (assoc "a" *registers* :test 'equal)) 0)
  (setf (cdr (assoc "b" *registers* :test 'equal)) 0))

(defun hlf (r &optional junk)
  (declare (ignore junk))
  (setf (cdr (assoc r *registers* :test 'equal)) (/ (cdr (assoc r *registers* :test 'equal)) 2))
  1)

(defun tpl (r &optional junk)
  (declare (ignore junk))
  (setf (cdr (assoc r *registers* :test 'equal)) (* (cdr (assoc r *registers* :test 'equal)) 3))
  1)

(defun inc (r &optional junk)
  (declare (ignore junk))
  (incf (cdr (assoc r *registers* :test 'equal)))
  1)

(defun jmp (offset &optional junk)
  (declare (ignore junk))
  offset)

(defun jie (register offset)
  (if (evenp (cdr (assoc register *registers* :test 'equal)))
      offset
      1))

(defun jio (register offset)
  (if (= 1 (cdr (assoc register *registers* :test 'equal)))
      offset
      1))

(defparameter *commands* '(("hlf" . hlf) ("tpl" . tpl) ("inc" . inc) ("jmp" . jmp) ("jie" . jie) ("jio" . jio)))

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

(defun perform-instruction (instruction)
  (let* ((cmd         (nth 0 instruction))
         (register    (nth 1 instruction))
         (jio-jie     (nth 2 instruction))
         (cmd-fn      (cdr (assoc cmd *commands* :test 'equal)))
         (next-offset (funcall cmd-fn register jio-jie)))
    (setf *inst-ptr* (+ *inst-ptr* next-offset))))

(defun execute-instructions ()
  (reset-registers)
  (setf *inst-ptr* 0)
  (loop :with end-of-instructions = (length *instructions*)
        :for instruction = (aref *instructions* *inst-ptr*) :then (aref *instructions* *inst-ptr*)
        :while (< *inst-ptr* end-of-instructions)
        :do (perform-instruction instruction))
  *registers*)

;; Part 2

;; What is register B if register A starts at 1 instead?

(defun reset-registers ()
  (setf (cdr (assoc "a" *registers* :test 'equal)) 1)
  (setf (cdr (assoc "b" *registers* :test 'equal)) 0))

;; Run execute-instructions again after compiling the above functions
