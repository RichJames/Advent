;;;;
;;;;  Advent of Code 2016
;;;;     Day 21

(defpackage #:2016Day21
  (:use #:cl :cl-ppcre))

(in-package #:2016Day21)

(defun swap-position (str x y)
  (let ((temp-str (copy-seq str))
        (char-x   (elt str (parse-integer x))))
    (setf (elt temp-str (parse-integer x)) (elt temp-str (parse-integer y)))
    (setf (elt temp-str (parse-integer y)) char-x)
    temp-str))

(defun swap-letters (str x y)
  (let ((pos-x    (position (coerce x 'character) str))
        (pos-y    (position (coerce y 'character) str)))
    (swap-position str (write-to-string pos-x) (write-to-string pos-y))))

(defun rotate (str s &key left)
  (let ((shift (mod (parse-integer s) (length str))))
    (if left
        (concatenate 'string (subseq str shift) (subseq str 0 shift))
        (concatenate 'string (subseq str (- (length str) shift)) (subseq str 0 (- (length str) shift))))))

(defun rotate-based (str x)
  (let ((pos-x  (position (coerce x 'character) str)))
    (rotate str (write-to-string (+ 1 pos-x (if (> pos-x 3) 1 0))))))

(defun rev (str x y)
  (let* ((x-int      (parse-integer x))
         (y-int      (parse-integer y))
         (safe-y     (if (>= y-int (length str)) (length str) (1+ y-int)))
         (substring  (reverse (subseq str x-int safe-y))))
    (concatenate 'string (subseq str 0 x-int) substring (subseq str safe-y))))

(defun move (str x y)
  (let* ((char-x   (elt str (parse-integer x)))
         (temp-str (remove char-x (copy-seq str) :count 1)))
    (concatenate 'string (subseq temp-str 0 (parse-integer y)) (coerce (list char-x) 'string) (subseq temp-str (parse-integer y)))))

(defun load-instructions (file)
  (with-open-file (stream file)

    (loop :for inst = (read-line stream nil nil)
          :while inst
          :collect (parse-instruction inst))))

(defun parse-instruction (string)
  (let ((regexes  (list "(move) position ([0-9]+) to position ([0-9])"
                        "(swap position) ([0-9]+) with position ([0-9]+)"
                        "(swap letter) ([a-z]) with letter ([a-z])"
                        "(reverse) positions ([0-9]+) through ([0-9]+)"
                        "(rotate) ([a-z]+) ([0-9]+) step"
                        "(rotate based) on position of letter ([a-z])")))

    (loop :for regex :in regexes
          :for parsed-inst = (multiple-value-list (ppcre:scan-to-strings regex string))
          :until (car parsed-inst)
          :finally (return (second parsed-inst)))))

(defun execute-instruction (string instruction)
  (cond ((null instruction)                            nil)
        ((string= (elt instruction 0) "move")          (move string (elt instruction 1) (elt instruction 2)))
        ((string= (elt instruction 0) "swap position") (swap-position string (elt instruction 1) (elt instruction 2)))
        ((string= (elt instruction 0) "swap letter")   (swap-letters string (elt instruction 1) (elt instruction 2)))
        ((string= (elt instruction 0) "reverse")       (rev string (elt instruction 1) (elt instruction 2)))
        ((string= (elt instruction 0) "rotate based")  (rotate-based string (elt instruction 1)))
        ((string= (elt instruction 0) "rotate")        (rotate string (elt instruction 2) :left (string= (elt instruction 1) "left")))))

(defun generate-password (string instructions)
  (cond ((null instructions) string)
        (t (generate-password (execute-instruction string (car instructions)) (cdr instructions)))))

(defun part1 ()
  (generate-password "abcdefgh" (load-instructions "~/quicklisp/local-projects/rich/advent/2016/Day21.txt")))





