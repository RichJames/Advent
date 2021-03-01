;;;;
;;;; Advent of Code 2015
;;;;   Day 16

(defpackage #:Day16
  (:use #:cl))

(in-package #:Day16)

;; An example aunt:
(defparameter *test-aunt* '(* * 9 * * * 9 * 0 *))

(defparameter *aunts* (make-array 500 :fill-pointer 0))

;; The MFCSAM result:
(defparameter *mfcsam-result* '(3 7 2 3 0 0 5 3 2 1))

(defun get-attribute (attribute data)
  (let* ((attr-length (+ (length attribute) 2))
         (attr-pos (search attribute data)))
    (if attr-pos
        (let* ((snippet (subseq data (+ attr-pos attr-length)))
               (comma-pos (position #\, snippet)))
          (if comma-pos
              (parse-integer (subseq snippet 0 comma-pos))
              (parse-integer snippet)))
        '*)))

(defun parse-input (data)
  (let ((attributes '("children" "cats" "samoyeds" "pomeranians" "akitas"
                      "vizslas" "goldfish" "trees" "cars" "perfumes")))
    (loop :for attribute :in attributes
          :collect (get-attribute attribute data))))

(defun init-aunts ()
  (setf (fill-pointer *aunts*) 0)
  (with-open-file (stream "~/quicklisp/local-projects/rich/advent/2015/Day16.txt")
    (loop :for data = (read-line stream nil nil)
          :while data
          :for aunt-data = (parse-input data)
          :do (vector-push aunt-data *aunts*))))

(defun matches-result-p (aunt result)
  (notany #'null (mapcar #'(lambda (x y) (if (or (equal x '*) (equal x y)) t nil)) aunt result)))

(defun find-matching-aunt (result)
  (loop :for aunt :across *aunts*
        :for i = 1 :then (1+ i)
        :if (matches-result-p aunt result)
          :return i))

;; Part2

;; Cats and trees readings indicate there are greater than that many (lower bound).
;; Permeranian and goldfish readings indicate there are fewer that that (upper bound).
;; Now find the matching aunt.

;; Match pattern (e=exact, u=upper-bound, l=lower-bound):
;; (e l e u e e u l e e)

(defun get-matches (aunt result)
  (let ((match-template '(e l e u e e u l e e)))
    (mapcar #'(lambda (x y template)
                (if (equal x '*)
                    t
                    (case template
                      (e (if (equal x y) t nil))
                      (l (if (> x y) t nil))
                      (u (if (< x y) t nil)))))
            aunt
            result
            match-template)))

(defun matches-result-p (aunt result)
  (notany #'null (get-matches aunt result)))
