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

(defun increment (char)
  (let ((char-num (char-code char)))
    (cond ((= char-num 122) #\a)
          ((or (= char-num 104)
               (= char-num 107)
               (= char-num 110)) (code-char (+ char-num 2)))
          (t (code-char (1+ char-num))))))

(defun incr-string (str)
  (loop :with next-c = #\a
        :for c :across (reverse str)
        :if (equal next-c #\a)
          :do (setf next-c (increment c)) :and
          :collect next-c :into results
        :else
          :collect c :into results
        :finally (return (coerce (reverse results) 'string))))

;; Predicate to test if string contains 'i', 'o' or 'l'. Our increment logic won't
;; introduce these into a string, but they could already exist in a string we
;; were given.
(defun valid-chars-p (string)
  (not (or (find #\i string)
           (find #\o string)
           (find #\l string))))

;; Predicate to test if string contains at least one straight of 3 or more characters.
(defun has-straight-p (string)
  (cond ((< (length string) 3) nil)
        ((and (= (- (char-code (char string 1))
                    (char-code (char string 0)))
                 1)
              (= (- (char-code (char string 2))
                    (char-code (char string 1)))
                 1)) t)
        (t (has-straight-p (subseq string 1)))))

;; Predicate to test if string contains at least two pairs that are not overlapping.
(defun has-2-pairs-p (string)
  (has-pairs-p string nil))

;; Helper function for finding two pairs
(defun has-pairs-p (string &optional (found-one nil))
  (cond ((< (length string) 2) nil)
        ((and found-one
              (equal (char string 0)
                     (char string 1))) t)
        ((equal (char string 0)
                (char string 1)) (has-pairs-p (subseq string 2) t))
        (found-one (has-pairs-p (subseq string 1) t))
        (t (has-pairs-p (subseq string 1)))))

;; Next password function.  This function has a chance of running into an infinite loop.
;; Some simple validation (e.g. at least 5 characters long) should prevent that.
(defun next-password (password)
  (loop :for new-pass = (incr-string password) :then (incr-string new-pass)
        :until (and (valid-chars-p new-pass)
                    (has-straight-p new-pass)
                    (has-2-pairs-p new-pass))
        :finally (return new-pass)))
