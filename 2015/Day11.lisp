;;;;
;;;; Advent of Code 2015
;;;;   Day 11

(defpackage #:Day11
  (:use #:cl))

(in-package #:Day11)

;; Now that I understand the rules, I need to write code that will do this.
#|
- all lower case
- 8 characters in length
- must contain at least one straight of 3 or more characters
- 'i', 'o', 'l' are not allowed
- must contain two doubled pairs (not overlapping)
- increment up to find the next conforming password
|#
 
(defparameter *chars-not-allowed* '(105 108 111))

(defun passw->list (password)
  (mapcar #'char-code (coerce password 'list)))

(defun list->passw (lst)
  (coerce (mapcar #'code-char lst) 'string))

(defun increment (char-num)
  (cond ((= char-num 122) 97)
        ((member (1+ char-num) *chars-not-allowed*) (+ char-num 2))
        (t (1+ char-num))))

(defun incr-list (lst)
  (reverse (incr-list-helper (reverse lst))))

(defun incr-list-helper (lst)
  (cond ((null lst) nil)
        ((= (car lst) 122) (append (list 97) (incr-list-helper (cdr lst))))
        (t (append (list (increment (car lst))) (cdr lst)))))

(defun valid-chars-p (lst)
  (not (intersection lst *chars-not-allowed*)))

(defun has-straight-p (lst)
  (cond ((< (length lst) 3) nil)
        ((and (= (- (third lst) (second lst)) 1)
              (= (- (second lst) (first lst)) 1)) t)
        (t (has-straight-p (cdr lst)))))

(defun has-pairs-p (lst &optional (found-one nil))
  (cond ((< (length lst) 2) nil)
        ((and found-one (equal (first lst) (second lst))) t)
        ((equal (first lst) (second lst)) (has-pairs-p (cddr lst) t))
        (found-one (has-pairs-p (cdr lst) t))
        (t (has-pairs-p (cdr lst)))))

(defun next-password (password)
  (loop :for new-pass = (incr-list (passw->list password)) :then (incr-list new-pass)
        :until (and (valid-chars-p new-pass)
                    (has-straight-p new-pass)
                    (has-pairs-p new-pass))
        :finally (return (list->passw new-pass))))
