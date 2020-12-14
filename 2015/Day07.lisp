;;;;
;;;; Advent of Code 2015
;;;   Day 7


;; Gates: AND, OR, LSHIFT, RSHIFT, NOT

;; Make a hash table of the wires and record their values
(defparameter *wires* (make-hash-table :test 'equalp))

;; Convert an instruction from our input file into a list form
;; we can work with.
(defun parse-instruction (instr)
  (let* ((instr-list (remove-if #'(lambda (x) (equal x "->"))
				(loop :for i = 0 :then (1+ j)
				      :as j = (position #\Space instr :start i)
				      :collect (subseq instr i j)
				      :while j)))
	 (target (car (reverse instr-list)))
	 (gate-instr (reverse (cdr (reverse instr-list)))))
    (list gate-instr target)))

;; Post processing an instruction to convert numbers that are in strings to
;; actual numbers (make for cleaner code later and probably improves performance
;; somewhat.) 
(defun interpret-instruction (instr)
  (let* ((gate-instr (car instr))
	 (target     (cadr instr))
	 (pos1       (nth 0 gate-instr))
	 (pos1-num   (get-number pos1))
	 (pos2       (nth 1 gate-instr))
	 (pos3       (nth 2 gate-instr))
	 (pos3-num   (get-number pos3)))
    (cond ((and (numberp pos1-num) (not pos2)) (list (list pos1-num) target))
	  ((numberp pos1-num)                  (list (list pos1-num pos2 pos3) target))
	  ((numberp pos3-num)                  (list (list pos1 pos2 pos3-num) target))
	  (t                                   (list (list pos1 pos2 pos3) target)))))

;; Helper function to convert a number in a string to an actual unsigned integer.
(defun get-number (str)
  (if str
      (let ((parse-val (parse-integer str :junk-allowed t)))
	(cond ((numberp parse-val) (s->us parse-val))
	      (t parse-val)))))

;; Helper function to update hash table with an instruction
(defun record-instruction (instr)
  (setf (gethash (cadr instr) *wires*) (car instr)))

;; Reads instructions in file, converts them and stores them in our hash table.
(defun store-instructions (file)
  (setf *wires* (make-hash-table :test 'equalp))
  (with-open-file (myfile file)
    (loop :for instr = (read-line myfile nil nil)
	  :while instr
	  :do (record-instruction (interpret-instruction (parse-instruction instr))))))

;; Same as above, but sets the hash table up according to the part 2 instructions.
(defun store-instructions-part2 (file)
  (store-instructions file)
  (let ((a-instr (gethash "a" *wires*))
	(a-result (measure-wire "a")))
    (progn
      (store-instructions file)
      (setf (gethash "a" *wires*) a-instr)
      (record-instruction (list (list a-result) "b")))))


;; This is the main logic for determining the value of a wire.  It has to handle
;; the variety of instructions that are found in the hash table.
;;
;; Possible forms (as specified in the instruction file):
;;  # -> wire
;;  wire1 -> wire2
;;  NOT wire1 -> wire2
;;  wire1 AND wire2 -> wire3
;;    #   AND wire1 -> wire2
;;  wire1 OR wire2 -> wire3
;;  wire1 RSHIFT # -> wire2
;;  wire1 LSHIFT # -> wire2
(defun measure-wire-helper (wire)
  (let* ((wire-val (gethash wire *wires*))
	 (pos1     (nth 0 wire-val))
	 (pos2     (nth 1 wire-val))
	 (pos3     (nth 2 wire-val)))
    (cond ((and (numberp pos1) (not pos2)) pos1)
	  ((equalp "NOT"    pos1)          (not-gate pos2))
	  ((equalp "AND"    pos2)          (and-gate pos1 pos3))
	  ((equalp "OR"     pos2)          (or-gate  pos1 pos3))
	  ((equalp "RSHIFT" pos2)          (rshift-gate pos1 pos3))
	  ((equalp "LSHIFT" pos2)          (lshift-gate pos1 pos3))
	  (t                               (measure-wire pos1)))))

;; This is essential for performance.  We update the hash table with computed
;; values for wires as we find them.  This helps avoid massive recursive
;; calculations.
(defun measure-wire (wire)
  (let ((result (measure-wire-helper wire)))
    (progn
      (record-instruction (list (list result) wire))
      result)))

;; A human-friendly wire result
(defun print-wire (wire)
  (let ((val (measure-wire wire)))
    (format t "~%~d, ~16,'0b" val val)))


;;; ***** Helper functions for doing bitwise logic *****

;; bitwise AND
(defun and-gate (a b)
  (cond ((and (numberp a) (numberp b)) (s->us (logand a b)))
	((numberp a) (s->us (logand a (measure-wire b))))
	((numberp b) (s->us (logand (measure-wire a) b)))
	(t (s->us (logand (measure-wire a) (measure-wire b))))))

;; bitwise OR
(defun or-gate (a b)
  (cond ((and (numberp a) (numberp b)) (s->us (logior a b)))
	((numberp a) (s->us (logior a (measure-wire b))))
	((numberp b) (s->us (logior (measure-wire a) b)))
	(t (s->us (logior (measure-wire a) (measure-wire b))))))

;; bitwise LSHIFT
(defun lshift-gate (a x)
  (cond ((numberp a) (s->us (ash a x)))
	(t (s->us (ash (measure-wire a) x)))))

;; bitwise RSHIFT
(defun rshift-gate (a x)
  (cond ((numberp a) (s->us (ash a (- x))))
	(t (s->us (ash (measure-wire a) (- x))))))

;; bitwise NOT
(defun not-gate (a)
  (if (numberp a)
      (s->us (lognot a))
      (s->us (lognot (measure-wire a)))))

;; Notes:
;;  An unsigned integer type has a type specifier of 'unsigned-byte or (unsigned-byte *) or
;;  (integer 0 *).
;;
;;  bit-vector is a vector with the element-type of 'bit.
;;  When printed, bit vectors are represented as #*<bits of the vector>
;;  There is a simple-bit-vector that has no fill pointer and is not adjustable.
;;  You can access bits in a bit array by using the bit function:
;;     (bit (setq ba (make-array 8 :element-type 'bit :initial-element 1)) 3) ===> 1
;;  Use sbit if working with simple-bit-vectors.
;;  The type, bit, is equivalent to the types (integer 0 1) and (unsigned-byte 1).
;;
;;  You can explicitly check the type of a variable with check-type. E.g.:
;;     (defun plus1 (arg)
;;       (check-type arg number)
;;       (1+ arg))
;;
;;  The plus1 function above will error if a non-number is passed via arg.
;;
;;  You can declare the input and output types of functions:
;;      (declaim (ftype (function (fixnum) string) dothing))
;;      (defun dothing (n)
;;        ;; convert integer n to a string)
;;
;;  With the above, we declared that function dothing takes as input a fixnum
;;  and returns a string.
;;
;;  We can declare the types of &key parameters:
;;      (declaim (ftype (function (string &key (:n integer))) foo))
;;      (defun foo (bar &key n) ...)
;;
;;  

;; This bit of code is used to convert signed integers into unsigned integers.
(defparameter *bitwidth* 16)
(defparameter *unsigned-mask* (1- (ash 1 *bitwidth*)))

(defun s->us (number)
  (logand number *unsigned-mask*))

